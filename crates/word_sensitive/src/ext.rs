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
use word_sensitive::trie::NodeExt;
use rustler::types::tuple::get_tuple;
use rustler::{NifResult, Term};
use rustler::error::Error;
use std::collections::HashMap;
#[derive(Clone)]
pub struct Ext{
	pub cates: HashMap<usize, usize>,
	pub len: usize
}
impl NodeExt for Ext{
	fn get_len(&self) -> usize{
	  self.len
	}
	fn get_weight(&self) -> usize{
		self.cates.values().cloned().sum()
	}
	fn get_cate(&self) -> usize{
		match self.cates.keys().min(){
			Some(v) => *v,
			None => 0
		}
	}
	fn eq(&self, other: &Self) -> bool{
		self.len == other.len
	}
}
impl <'a> rustler::Decoder<'a> for Ext{
	fn decode(term: Term<'a>) -> NifResult<Self> {
		if term.is_tuple(){
			match get_tuple(term){
				Ok(t) => {
					Ok(Ext{
						cates:t[1].decode().unwrap(),
						len:t[2].decode().unwrap()
					})
				},
				Err(_) =>
				Err(Error::BadArg)
			}
		}else{
			Err(Error::BadArg)
		}
	}
}