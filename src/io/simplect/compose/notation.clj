;   Copyright (c) Klaus Harbo. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns io.simplect.compose.notation
  (:require
   [cats.core					:as cats]
   [clojure.core				:as core]
   [io.simplect.compose				:as ioc]
   [io.simplect.compose.util					:refer [fref var-arglist-and-doc merge-meta]]))

(fref Π	core/partial)
(fref π		ioc/partial>)
(fref γ		core/comp)
(fref Γ	ioc/rcomp)
(fref μ	core/map)
(fref μμ	core/mapv)
(fref ρ	core/reduce)
(fref conj-ρ	ioc/conjreduce)
(fref assoc-ρ	ioc/assocreduce)

(defmacro Ξ
  [& args]
  `(cats/curry ~@args))
(merge-meta #'Ξ (var-arglist-and-doc #'cats/curry))

(defmacro λ
  [& args]
  `(fn ~@args))
(merge-meta #'λ (var-arglist-and-doc #'fn))
