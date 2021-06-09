extern crate core;
extern crate rustler;
extern crate serde;
extern crate word_sensitive;
mod atoms;
mod options;
mod nif;
// define nif api
rustler::init!(
    "word_sensitive_nif",
    [
        nif::new,
        nif::clear,
        nif::add_key_word,
        nif::build,
        nif::query,
    ],
    load = nif::on_load
);