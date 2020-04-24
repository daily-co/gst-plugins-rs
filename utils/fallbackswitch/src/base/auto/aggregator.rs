// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use super::super::gst_base_sys;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use glib::StaticType;
use glib::Value;
use std::boxed::Box as Box_;
use std::mem::transmute;

glib_wrapper! {
    pub struct Aggregator(Object<gst_base_sys::GstAggregator, gst_base_sys::GstAggregatorClass, AggregatorClass>) @extends gst::Element, gst::Object;

    match fn {
        get_type => || gst_base_sys::gst_aggregator_get_type(),
    }
}

unsafe impl Send for Aggregator {}
unsafe impl Sync for Aggregator {}

pub const NONE_AGGREGATOR: Option<&Aggregator> = None;

pub trait AggregatorExt: 'static {
    //fn get_allocator(&self, allocator: /*Ignored*/gst::Allocator, params: /*Ignored*/gst::AllocationParams);

    fn get_buffer_pool(&self) -> Option<gst::BufferPool>;

    fn get_latency(&self) -> gst::ClockTime;

    fn set_latency(&self, min_latency: gst::ClockTime, max_latency: gst::ClockTime);

    fn set_src_caps(&self, caps: &gst::Caps);

    fn simple_get_next_time(&self) -> gst::ClockTime;

    fn get_property_start_time(&self) -> u64;

    fn set_property_start_time(&self, start_time: u64);

    fn connect_property_latency_notify<F: Fn(&Self) + Send + Sync + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;

    fn connect_property_start_time_notify<F: Fn(&Self) + Send + Sync + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;

    fn negotiate(&self) -> bool;
}

impl<O: IsA<Aggregator>> AggregatorExt for O {
    //fn get_allocator(&self, allocator: /*Ignored*/gst::Allocator, params: /*Ignored*/gst::AllocationParams) {
    //    unsafe { TODO: call gst_base_sys:gst_aggregator_get_allocator() }
    //}

    fn get_buffer_pool(&self) -> Option<gst::BufferPool> {
        unsafe {
            from_glib_full(gst_base_sys::gst_aggregator_get_buffer_pool(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_latency(&self) -> gst::ClockTime {
        unsafe {
            from_glib(gst_base_sys::gst_aggregator_get_latency(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn set_latency(&self, min_latency: gst::ClockTime, max_latency: gst::ClockTime) {
        unsafe {
            gst_base_sys::gst_aggregator_set_latency(
                self.as_ref().to_glib_none().0,
                min_latency.to_glib(),
                max_latency.to_glib(),
            );
        }
    }

    fn set_src_caps(&self, caps: &gst::Caps) {
        unsafe {
            gst_base_sys::gst_aggregator_set_src_caps(
                self.as_ref().to_glib_none().0,
                caps.to_glib_none().0,
            );
        }
    }

    fn simple_get_next_time(&self) -> gst::ClockTime {
        unsafe {
            from_glib(gst_base_sys::gst_aggregator_simple_get_next_time(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_property_start_time(&self) -> u64 {
        unsafe {
            let mut value = Value::from_type(<u64 as StaticType>::static_type());
            gobject_sys::g_object_get_property(
                self.to_glib_none().0 as *mut gobject_sys::GObject,
                b"start-time\0".as_ptr() as *const _,
                value.to_glib_none_mut().0,
            );
            value
                .get()
                .expect("Return Value for property `start-time` getter")
                .unwrap()
        }
    }

    fn set_property_start_time(&self, start_time: u64) {
        unsafe {
            gobject_sys::g_object_set_property(
                self.to_glib_none().0 as *mut gobject_sys::GObject,
                b"start-time\0".as_ptr() as *const _,
                Value::from(&start_time).to_glib_none().0,
            );
        }
    }

    fn connect_property_latency_notify<F: Fn(&Self) + Send + Sync + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn notify_latency_trampoline<P, F: Fn(&P) + Send + Sync + 'static>(
            this: *mut gst_base_sys::GstAggregator,
            _param_spec: glib_sys::gpointer,
            f: glib_sys::gpointer,
        ) where
            P: IsA<Aggregator>,
        {
            let f: &F = &*(f as *const F);
            f(&Aggregator::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::latency\0".as_ptr() as *const _,
                Some(transmute(notify_latency_trampoline::<Self, F> as usize)),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_property_start_time_notify<F: Fn(&Self) + Send + Sync + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn notify_start_time_trampoline<P, F: Fn(&P) + Send + Sync + 'static>(
            this: *mut gst_base_sys::GstAggregator,
            _param_spec: glib_sys::gpointer,
            f: glib_sys::gpointer,
        ) where
            P: IsA<Aggregator>,
        {
            let f: &F = &*(f as *const F);
            f(&Aggregator::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::start-time\0".as_ptr() as *const _,
                Some(transmute(notify_start_time_trampoline::<Self, F> as usize)),
                Box_::into_raw(f),
            )
        }
    }

    fn negotiate(&self) -> bool {
        unsafe {
            from_glib(gst_base_sys::gst_aggregator_negotiate(
                self.as_ref().to_glib_none().0,
            ))
        }
    }
}
