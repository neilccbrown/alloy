-- Alloy.
-- Copyright (c) 2008-2009, University of Kent.
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * Neither the name of the University of Kent nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | Alloy is a generic programming system for automatically traversing data
-- structures, operating on specific types within that structure.
--
-- To use the Alloy module, you can either use the helper functions from the
-- "Data.Generics.Alloy.Schemes" module or the lower-level functions from
-- "Data.Generics.Alloy.Pure" and "Data.Generics.Alloy.Effect".  The tutorial
-- (<http://twistedsquare.com/Alloy-Tutorial.pdf>) provides examples of each
-- of these.  The tutorial also explains how to use the "Data.Generics.Alloy.GenInstances"
-- module to generate the instances that Alloy needs for your data.

module Data.Generics.Alloy (
  module Data.Generics.Alloy.Pure,
  module Data.Generics.Alloy.Effect,
  module Data.Generics.Alloy.Route,
  module Data.Generics.Alloy.Schemes,
  ) where

import Data.Generics.Alloy.Pure
import Data.Generics.Alloy.Effect
import Data.Generics.Alloy.Route
import Data.Generics.Alloy.Schemes

