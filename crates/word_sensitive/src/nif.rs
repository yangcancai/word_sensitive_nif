//-------------------------------------------------------------------
// @author yangcancai

// Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//       https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
  
// @doc
//
// @end
// Created : 2021-06-11T07:21:46+00:00
//-------------------------------------------------------------------

use std::{sync::{RwLock, RwLockReadGuard, RwLockWriteGuard}};
use std::borrow::Cow;
use rustler::resource::ResourceArc;
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term};

use atoms::{ok};
use options::NifwordSensitiveOptions;
use word_sensitive::Trie;
use ext::Ext;
use std::collections::HashMap;
// =================================================================================================
// resource
// =================================================================================================
struct NifwordSensitive{
    data: Trie<Ext>
}
impl NifwordSensitive{
    // create
    fn new(_: NifwordSensitiveOptions) -> Result<Self, String>{
        Ok(NifwordSensitive{data: Trie::default()})
    }
    // clear
    fn clear(&mut self) {
    }
    fn add_key_word_ext(&mut self, msg: &[u8], ext: Ext) {
        self.data.add_key_word_ext(msg.to_vec(), ext);
    }
    fn build(&mut self){
        self.data.build();
    }
    fn query<'a>(&self, text: &'a [u8]) -> Vec<String> {
        self.data.query(text).iter().map(|x|self.u8_to_string(x)).collect()
    }
    fn query_total_weight(&self, text: &[u8]) -> usize {
        self.data.query_total_weight(text) 
    }
    fn query_cate_weight(&self, text: &[u8]) -> HashMap<usize,usize>{
        self.data.query_cate_weight(text) 
    }
    fn u8_to_string(&self, msg: &[u8]) -> String{
        let a = String::from_utf8_lossy(msg);
        match a{
            Cow::Owned(own_msg) => own_msg,
            Cow::Borrowed(b_msg) => b_msg.to_string()
        }
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
    rs.add_key_word_ext(&keyword, Ext{cate:1,len: keyword.len(), weight:1});
   Ok(ok().encode(env)) 
}
#[rustler::nif]
fn add_key_word_ext<'a>(env: Env<'a>, resource: ResourceArc<NifwordSensitiveResource>, keyword: LazyBinary<'a>, ext: Ext) -> NifResult<Term<'a>> {
    let mut rs = resource.write();
    rs.add_key_word_ext(&keyword, ext);
   Ok(ok().encode(env)) 
}
#[rustler::nif]
fn query<'a>(env: Env<'a>, resource: ResourceArc<NifwordSensitiveResource>, text: LazyBinary<'a>) -> NifResult<Term<'a>> {
    let rs = resource.read();
    let list = rs.query(&text);
    Ok(list.encode(env))
}
#[rustler::nif]
fn query_total_weight<'a>(env: Env<'a>, resource: ResourceArc<NifwordSensitiveResource>, text: LazyBinary<'a>) -> NifResult<Term<'a>> {
    let rs = resource.read();
    let list = rs.query_total_weight(&text);
    Ok(list.encode(env))
}
#[rustler::nif]
fn query_cate_weight<'a>(env: Env<'a>, resource: ResourceArc<NifwordSensitiveResource>, text: LazyBinary<'a>) -> NifResult<Term<'a>> {
    let rs = resource.read();
    let list = rs.query_cate_weight(&text);
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
