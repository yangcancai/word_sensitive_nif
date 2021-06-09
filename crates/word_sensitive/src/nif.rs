use std::{sync::{RwLock, RwLockReadGuard, RwLockWriteGuard}};

use rustler::resource::ResourceArc;
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term};

use atoms::{ok};
use options::NifwordSensitiveOptions;
use word_sensitive::Trie;
// =================================================================================================
// resource
// =================================================================================================
struct NifwordSensitive{
    data: Trie 
}
impl NifwordSensitive{
    // create
    fn new(_: NifwordSensitiveOptions) -> Result<Self, String>{
        Ok(NifwordSensitive{data: Trie::default()})
    }
    // clear
    fn clear(&mut self) {
    }
    // write
    fn add_key_word(&mut self, msg: &[u8]) {
        self.data.add_key_word(msg.to_vec());
    }
    fn build(&mut self){
        self.data.build();
    }
    fn query<'a>(&self, text: &'a [u8]) -> Vec<&'a [u8]> {
        self.data.query(text)
    }
}
#[repr(transparent)]
struct NifwordSensitiveResource(RwLock<NifwordSensitive>);

impl NifwordSensitiveResource {
    fn read(&self) -> RwLockReadGuard<'_, NifwordSensitive> {
        self.0.read().unwrap()
    }

    fn write(&self) -> RwLockWriteGuard<'_, NifwordSensitive> {
        self.0.write().unwrap()
    }
}

impl From<NifwordSensitive> for NifwordSensitiveResource {
    fn from(other: NifwordSensitive) -> Self {
        NifwordSensitiveResource(RwLock::new(other))
    }
}

pub fn on_load(env: Env, _load_info: Term) -> bool {
    rustler::resource!(NifwordSensitiveResource, env);
    true
}

// =================================================================================================
// api
// =================================================================================================

#[rustler::nif]
fn new(env: Env, opts: NifwordSensitiveOptions) -> NifResult<Term> {
    let rs = NifwordSensitive::new(opts).map_err(|e| rustler::error::Error::Term(Box::new(e)))?;
    Ok((ok(), ResourceArc::new(NifwordSensitiveResource::from(rs))).encode(env))
}
#[rustler::nif]
fn clear(env: Env, resource: ResourceArc<NifwordSensitiveResource>) -> NifResult<Term> {
    resource.write().clear();
    Ok(ok().encode(env))
}
#[rustler::nif]
fn build(env: Env, resource: ResourceArc<NifwordSensitiveResource>) -> NifResult<Term> {
    let mut rs = resource.write();
    rs.build();
   Ok(ok().encode(env)) 
}
#[rustler::nif]
fn add_key_word<'a>(env: Env<'a>, resource: ResourceArc<NifwordSensitiveResource>, keyword: LazyBinary<'a>) -> NifResult<Term<'a>> {
    let mut rs = resource.write();
    rs.add_key_word(&keyword);
   Ok(ok().encode(env)) 
}
#[rustler::nif]
fn query<'a>(env: Env<'a>, resource: ResourceArc<NifwordSensitiveResource>, text: LazyBinary<'a>) -> NifResult<Term<'a>> {
    let rs = resource.read();
    let list = rs.query(&text);
    Ok(list.encode(env))
}

// =================================================================================================
// helpers
// =================================================================================================

/// Represents either a borrowed `Binary` or `OwnedBinary`.
///
/// `LazyBinary` allows for the most efficient conversion from an
/// Erlang term to a byte slice. If the term is an actual Erlang
/// binary, constructing `LazyBinary` is essentially
/// zero-cost. However, if the term is any other Erlang type, it is
/// converted to an `OwnedBinary`, which requires a heap allocation.
enum LazyBinary<'a> {
    Owned(OwnedBinary),
    Borrowed(Binary<'a>),
}

impl<'a> std::ops::Deref for LazyBinary<'a> {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        match self {
            Self::Owned(owned) => owned.as_ref(),
            Self::Borrowed(borrowed) => borrowed.as_ref(),
        }
    }
}

impl<'a> rustler::Decoder<'a> for LazyBinary<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if term.is_binary() {
            Ok(Self::Borrowed(Binary::from_term(term)?))
        } else {
            Ok(Self::Owned(term.to_binary()))
        }
    }
}
