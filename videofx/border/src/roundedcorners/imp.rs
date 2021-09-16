// Copyright (C) 2021 Asymptotic Inc.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use atomic_refcell::AtomicRefCell;
use gst::{glib, gst_debug, gst_info, subclass::prelude::*};
use gst_base::{
    prelude::*,
    subclass::base_transform::{Buffer, PrepareOutputBufferSuccess},
};
use gst_video::{subclass::prelude::*, VideoFormat, VideoFrameRef};

use once_cell::sync::Lazy;
use std::sync::Mutex;

const DEFAULT_BORDER_RADIUS: u32 = 0;

static CAT: Lazy<gst::DebugCategory> = Lazy::new(|| {
    gst::DebugCategory::new(
        "roundedcorners",
        gst::DebugColorFlags::empty(),
        Some("Rounded corners"),
    )
});

#[derive(Debug, Clone, Copy)]
struct Settings {
    border_radius_px: u32,
    changed: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            border_radius_px: DEFAULT_BORDER_RADIUS,
            changed: false,
        }
    }
}

struct State {
    alpha_buffer: AtomicRefCell<Vec<u8>>,
    out_info: Option<gst_video::VideoInfo>,
}

#[derive(Default)]
pub struct RoundedCorners {
    settings: Mutex<Settings>,
    state: Mutex<Option<State>>,
}

impl RoundedCorners {
    fn draw_rounded_corners(
        &self,
        cairo_ctx: &cairo::Context,
        border_radius_px: u32,
        width: f64,
        height: f64,
    ) -> Result<(), cairo::Error> {
        let border_radius = border_radius_px as f64;
        let degrees = std::f64::consts::PI / 180.0;

        // Taken from https://www.cairographics.org/samples/rounded_rectangle/
        cairo_ctx.new_sub_path();
        cairo_ctx.arc(
            width - border_radius,
            border_radius,
            border_radius,
            -90_f64 * degrees,
            0 as f64 * degrees,
        );
        cairo_ctx.arc(
            width - border_radius,
            height - border_radius,
            border_radius,
            0 as f64 * degrees,
            90_f64 * degrees,
        );
        cairo_ctx.arc(
            border_radius,
            height - border_radius,
            border_radius,
            90_f64 * degrees,
            180_f64 * degrees,
        );
        cairo_ctx.arc(
            border_radius,
            border_radius,
            border_radius,
            180_f64 * degrees,
            270_f64 * degrees,
        );
        cairo_ctx.close_path();

        cairo_ctx.set_source_rgb(0.0, 0.0, 0.0);
        cairo_ctx.fill_preserve()?;
        cairo_ctx.set_source_rgba(0.0, 0.0, 0.0, 1.0);
        cairo_ctx.set_line_width(1.0);
        cairo_ctx.stroke()?;

        Ok(())
    }

    fn generate_alpha_mask(&self, border_radius_px: u32) -> Result<(), gst::LoggableError> {
        let state_guard = self
            .state
            .lock()
            .map_err(|_| gst::loggable_error!(CAT, "Failed to get state lock"))?;
        let out_info = state_guard.as_ref().unwrap().out_info.as_ref().unwrap();

        let width = out_info.width() as i32;
        let height = out_info.height() as i32;
        let alpha_stride = out_info.stride()[3];
        let mut alpha_buffer = state_guard.as_ref().unwrap().alpha_buffer.borrow_mut();

        alpha_buffer.clear();
        alpha_buffer.resize((alpha_stride * height) as usize, 0);

        let surface = unsafe {
            cairo::ImageSurface::create_for_data_unsafe(
                alpha_buffer.as_mut_slice().as_mut_ptr(),
                cairo::Format::A8,
                width,
                height,
                alpha_stride,
            )
        };

        if let Err(e) = surface {
            return Err(gst::loggable_error!(
                CAT,
                "Failed to create cairo image surface: {}",
                e
            ));
        }

        match cairo::Context::new(surface.as_ref().unwrap()) {
            Ok(cr) => {
                if let Err(e) =
                    self.draw_rounded_corners(&cr, border_radius_px, width as f64, height as f64)
                {
                    return Err(gst::loggable_error!(
                        CAT,
                        "Failed to draw rounded corners: {}",
                        e
                    ));
                };

                drop(cr);

                unsafe {
                    assert_eq!(
                        cairo::ffi::cairo_surface_get_reference_count(
                            surface.unwrap().to_raw_none()
                        ),
                        1
                    );
                }

                Ok(())
            }
            Err(e) => Err(gst::loggable_error!(
                CAT,
                "Failed to create cairo context: {}",
                e
            )),
        }
    }

    fn copy_and_append_alpha(
        &self,
        outframe: &mut VideoFrameRef<&mut gst::BufferRef>,
    ) -> Result<gst::FlowSuccess, gst::FlowError> {
        match &*self.state.lock().unwrap() {
            Some(state) => {
                let alpha_plane = outframe.plane_data_mut(3).unwrap();
                alpha_plane.copy_from_slice(state.alpha_buffer.borrow().as_slice());
                Ok(gst::FlowSuccess::Ok)
            }
            None => {
                gst::loggable_error!(CAT, "Missing alpha buffer!!!");
                Err(gst::FlowError::NotNegotiated)
            }
        }
    }
}

#[glib::object_subclass]
impl ObjectSubclass for RoundedCorners {
    const NAME: &'static str = "RoundedCorners";
    type Type = super::RoundedCorners;
    type ParentType = gst_base::BaseTransform;
}

impl ObjectImpl for RoundedCorners {
    fn properties() -> &'static [glib::ParamSpec] {
        static PROPERTIES: Lazy<Vec<glib::ParamSpec>> = Lazy::new(|| {
            vec![glib::ParamSpec::new_uint(
                "border-radius-px",
                "Border radius in pixels",
                "Draw rounded corners with given border radius",
                0,
                u32::MAX,
                DEFAULT_BORDER_RADIUS,
                glib::ParamFlags::READWRITE | gst::PARAM_FLAG_MUTABLE_PLAYING,
            )]
        });

        PROPERTIES.as_ref()
    }

    fn set_property(
        &self,
        obj: &Self::Type,
        _id: usize,
        value: &glib::Value,
        pspec: &glib::ParamSpec,
    ) {
        match pspec.name() {
            "border-radius-px" => {
                let mut settings = self.settings.lock().unwrap();
                let border_radius = value.get().expect("type checked upstream");
                if settings.border_radius_px != border_radius {
                    settings.changed = true;
                    settings.border_radius_px = border_radius;
                    gst_info!(
                        CAT,
                        obj: obj,
                        "Changing border radius from {} to {}",
                        settings.border_radius_px,
                        border_radius
                    );
                    BaseTransformExt::reconfigure_src(obj);
                }
            }
            _ => unimplemented!(),
        }
    }

    fn property(&self, _obj: &Self::Type, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
        match pspec.name() {
            "border-radius-px" => {
                let settings = self.settings.lock().unwrap();
                settings.border_radius_px.to_value()
            }
            _ => unimplemented!(),
        }
    }
}

impl ElementImpl for RoundedCorners {
    fn metadata() -> Option<&'static gst::subclass::ElementMetadata> {
        static ELEMENT_METADATA: Lazy<gst::subclass::ElementMetadata> = Lazy::new(|| {
            gst::subclass::ElementMetadata::new(
                "Rounded Corners",
                "Filter/Effect/Converter/Video",
                "Adds rounded corners to video",
                "Sanchayan Maity <sanchayan@asymptotic.io>",
            )
        });

        Some(&*ELEMENT_METADATA)
    }

    fn pad_templates() -> &'static [gst::PadTemplate] {
        static PAD_TEMPLATES: Lazy<Vec<gst::PadTemplate>> = Lazy::new(|| {
            let sink_caps = gst::Caps::builder("video/x-raw")
                .field("format", &VideoFormat::I420.to_str())
                .field("width", &gst::IntRange::<i32>::new(1, i32::MAX))
                .field("height", &gst::IntRange::<i32>::new(1, i32::MAX))
                .field(
                    "framerate",
                    &gst::FractionRange::new(
                        gst::Fraction::new(0, 1),
                        gst::Fraction::new(i32::MAX, 1),
                    ),
                )
                .build();
            let sink_pad_template = gst::PadTemplate::new(
                "sink",
                gst::PadDirection::Sink,
                gst::PadPresence::Always,
                &sink_caps,
            )
            .unwrap();

            let src_caps = gst::Caps::builder("video/x-raw")
                .field(
                    "format",
                    &gst::List::new(&[&VideoFormat::A420.to_str(), &VideoFormat::I420.to_str()]),
                )
                .field("width", &gst::IntRange::<i32>::new(1, i32::MAX))
                .field("height", &gst::IntRange::<i32>::new(1, i32::MAX))
                .field(
                    "framerate",
                    &gst::FractionRange::new(
                        gst::Fraction::new(0, 1),
                        gst::Fraction::new(i32::MAX, 1),
                    ),
                )
                .build();
            let src_pad_template = gst::PadTemplate::new(
                "src",
                gst::PadDirection::Src,
                gst::PadPresence::Always,
                &src_caps,
            )
            .unwrap();

            vec![sink_pad_template, src_pad_template]
        });

        PAD_TEMPLATES.as_ref()
    }
}

impl BaseTransformImpl for RoundedCorners {
    const MODE: gst_base::subclass::BaseTransformMode = gst_base::subclass::BaseTransformMode::Both;
    const PASSTHROUGH_ON_SAME_CAPS: bool = false;
    const TRANSFORM_IP_ON_PASSTHROUGH: bool = false;

    fn start(&self, element: &Self::Type) -> Result<(), gst::ErrorMessage> {
        *self.state.lock().unwrap() = Some(State {
            alpha_buffer: AtomicRefCell::new(Vec::new()),
            out_info: None,
        });

        gst_info!(CAT, obj: element, "Started");

        Ok(())
    }

    fn stop(&self, element: &Self::Type) -> Result<(), gst::ErrorMessage> {
        let _ = self.state.lock().unwrap().take();

        gst_info!(CAT, obj: element, "Stopped");

        Ok(())
    }

    fn transform_caps(
        &self,
        element: &Self::Type,
        direction: gst::PadDirection,
        caps: &gst::Caps,
        filter: Option<&gst::Caps>,
    ) -> Option<gst::Caps> {
        let other_caps = if direction == gst::PadDirection::Src {
            let mut caps = caps.clone();

            for s in caps.make_mut().iter_mut() {
                s.set("format", &VideoFormat::I420.to_str());
            }

            caps
        } else {
            let mut output_caps = gst::Caps::new_empty();
            {
                let output_caps = output_caps.get_mut().unwrap();
                let border_radius = self.settings.lock().unwrap().border_radius_px;

                for s in caps.iter() {
                    let mut s_output = s.to_owned();
                    if border_radius == 0 {
                        s_output.set(
                            "format",
                            &gst::List::new(&[
                                &VideoFormat::I420.to_str(),
                                &VideoFormat::A420.to_str(),
                            ]),
                        );
                        BaseTransformExt::set_passthrough(element, true);
                    } else {
                        s_output.set(
                            "format",
                            &gst::List::new(&[
                                &VideoFormat::A420.to_str(),
                                &VideoFormat::I420.to_str(),
                            ]),
                        );
                    }
                    output_caps.append_structure(s_output);
                }
            }

            output_caps
        };

        gst_debug!(
            CAT,
            obj: element,
            "Transformed caps from {} to {} in direction {:?}",
            caps,
            other_caps,
            direction
        );

        if let Some(filter) = filter {
            Some(filter.intersect_with_mode(&other_caps, gst::CapsIntersectMode::First))
        } else {
            Some(other_caps)
        }
    }

    fn set_caps(
        &self,
        element: &Self::Type,
        incaps: &gst::Caps,
        outcaps: &gst::Caps,
    ) -> Result<(), gst::LoggableError> {
        let mut settings = self.settings.lock().unwrap();

        let out_info = match gst_video::VideoInfo::from_caps(outcaps) {
            Err(_) => return Err(gst::loggable_error!(CAT, "Failed to parse output caps")),
            Ok(info) => info,
        };

        gst_debug!(
            CAT,
            obj: element,
            "Configured for caps {} to {}",
            incaps,
            outcaps
        );

        let mut state_guard = self.state.lock().unwrap();
        let state = state_guard
            .as_mut()
            .ok_or_else(|| gst::loggable_error!(CAT, "Have no state yet"))?;

        state.out_info = Some(out_info);

        settings.changed = true;

        Ok(())
    }

    fn prepare_output_buffer(
        &self,
        element: &Self::Type,
        inbuf: Buffer,
    ) -> Result<PrepareOutputBufferSuccess, gst::FlowError> {
        let settings = self.settings.lock().unwrap();
        match settings.border_radius_px {
            0 => Ok(PrepareOutputBufferSuccess::InputBuffer),
            _ => {
                let state_guard = self.state.lock().unwrap();
                let state = state_guard.as_ref().ok_or_else(|| {
                    gst::element_error!(
                        element,
                        gst::CoreError::Negotiation,
                        ["Have no state yet"]
                    );
                    gst::FlowError::NotNegotiated
                })?;

                let size = state.out_info.as_ref().unwrap().size() as usize;
                let alpha_plane = gst::Memory::with_size(size);

                match inbuf {
                    Buffer::Writable(outbuf) => {
                        outbuf.append_memory(alpha_plane);

                        Ok(PrepareOutputBufferSuccess::Buffer(outbuf.to_owned()))
                    }
                    Buffer::Readable(buf) => {
                        let mut outbuf = buf.copy();
                        let mut_outbuf = outbuf.make_mut();
                        mut_outbuf.append_memory(alpha_plane);

                        Ok(PrepareOutputBufferSuccess::Buffer(outbuf))
                    }
                }
            }
        }
    }

    fn transform(
        &self,
        element: &Self::Type,
        _inbuf: &gst::Buffer,
        outbuf: &mut gst::BufferRef,
    ) -> Result<gst::FlowSuccess, gst::FlowError> {
        let mut settings = self.settings.lock().unwrap();
        if settings.changed {
            settings.changed = false;
            gst_debug!(
                CAT,
                obj: element,
                "Caps or border radius changed, generating alpha mask"
            );
            if self.generate_alpha_mask(settings.border_radius_px).is_err() {
                gst::element_error!(
                    element,
                    gst::CoreError::Negotiation,
                    ["Failed to generate alpha mask"]
                );
                return Err(gst::FlowError::NotNegotiated);
            }
        }

        let state_guard = self.state.lock().unwrap();
        let state = state_guard.as_ref().ok_or_else(|| {
            gst::element_error!(element, gst::CoreError::Negotiation, ["Have no state yet"]);
            gst::FlowError::NotNegotiated
        })?;
        let mut out_frame = gst_video::VideoFrameRef::from_buffer_ref_writable(
            outbuf,
            state.out_info.as_ref().unwrap(),
        )
        .map_err(|_| {
            gst::element_error!(
                element,
                gst::CoreError::Failed,
                ["Failed to map output buffer writable"]
            );
            gst::FlowError::Error
        })?;

        drop(state_guard);

        self.copy_and_append_alpha(&mut out_frame)?;

        Ok(gst::FlowSuccess::Ok)
    }
}
