{-# LANGUAGE BangPatterns #-}
{-|
Module      : GameSearch
Description : Mostly general-purpose implementation of multiplayer Monte Carlo Tree Search
Copyright   : (c) Avinash Dwarapu, 2017
License     : BSD-3-Clause
Maintainer  : 
Stability   : experimental
Portability : POSIX

Copyright (c) 2017, Avinash Dwarapu

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Avinash Dwarapu nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module GameSearch
    ( module GameSearch.Core
    , timedMCTS
    ) where

import GameSearch.Core
import System.CPUTime (getCPUTime)
import System.Random  (StdGen)

-- | Run monte carlo search for a given number of seconds.
timedMCTS ::
    Spec s a p => Double -> StdGen -> s -> Node a p -> IO (Node a p)
timedMCTS seconds r s n = do
    curTime <- getCPUTime
    timedMCTS' (curTime + floor (seconds * 1000000000000)) r s n

-- | Run monte carlo search until cpuTime hits a certain value. Prevents issues
-- clock drift during computation. BangPatterns prevents lazy computation so
-- that the clock ticks as expected.
timedMCTS' ::
    Spec s a p => Integer -> StdGen -> s -> Node a p -> IO (Node a p)
timedMCTS' stopTime r s n = do
    let !(nr, nn) = monteCarlo r s n
    curTime <- getCPUTime
    if stopTime >= curTime
        then timedMCTS' stopTime nr s nn
        else return nn
