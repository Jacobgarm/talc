//use malachite::{
//    num::conversion::traits::{ConvertibleFrom, SaturatingFrom},
//    Integer,
//};
//
//pub fn try_int_to_unsigned(int: &Integer) -> Option<u64> {
//    if u64::convertible_from(int) {
//        Some(u64::saturating_from(int))
//    } else {
//        None
//    }
//}
//
//pub fn try_int_to_signed(int: &Integer) -> Option<i64> {
//    if i64::convertible_from(int) {
//        Some(i64::saturating_from(int))
//    } else {
//        None
//    }
//}
