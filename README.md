# cljsl
DISCLAIMER: CLJSL is pre-alpha software, and is subject to change in future.
This document gives an introduction to using the current version of it at time
of writing, but may become out of date.

CLJSL (the Clojure Shader Language) is a library for compiling a subset of
Clojure code to compliant GLSL code for use with OpenGL, OpenGL ES, Vulkan (via
a GLSL->SPIRV compiler), and WebGL. It provides facilities beyond simple
translation of code as well, including dependency tracking, namespacing, and
macros. CLJSL code is writtin as normal code inside of your existing Clojure
namespaces, no resource files or similar are needed.

At the current stage, CLJSL is simplistic and does not emit errors from bad code
to be compiled, instead leaving those errors to be picked up by the GLSL
compiler. For example, CLJSL does not emit an error when using `if` in a part of
the code besides a statement, but it does not support `if` expressions. Errors
in usage of CLJSL functions and macros, however, are reported to the user via
specs on macros and exceptions when calling compilation functions directly.

## Installation
As CLJSL is pre-alpha software, no Clojars release has been made. You can depend
on it as a git dependency when using the Clojure CLI, or you can manually build
a jar and install it to your local maven repository (although CLJSL currently
provides no build process for jars).

## Usage
CLJSL provides a set of macros which can be used to build shaders. Each one
registers a var that tracks which others it depends on to be included in the
final shader source. To begin with, we'll make a simple pass-through vertex
shader.

``` clojure
(require '[cljsl.compiler :as sl])

(sl/defparam vertex-position "vec3"
  :layout {:location 0})

(sl/defparam vertex-color "vec3"
  :layout {:location 1})

(sl/defparam geometry-color "vec3")

(sl/defshader vertex-shader
  {vertex-position :in
   vertex-color :in
   geometry-color :out}
  (set! gl_Position (vec4 vertex-position (float 1)))
  (set! geometry-color vertex-color))
```

To start with, we require the CLJSL compiler, and then we define the shader
parameters. These are variables which can be used across different shaders to
pass values between them.

The parameters `vertex-position` and `vertex-color` are given layout maps which
specify a location because we will use them to get the per-vertex data from the
application.

The shader itself includes a map to specify which parameters it will be using,
as well as whether they will be used as input or output parameters for this
shader. This is specified at the shader level because multiple shaders may refer
to the same parameters with a different choice of input vs output, as will be
shown shortly. Next it uses the `set!` operator to set the values of
`gl_Position`, a variable provided by GLSL, to the return value of a call to
`vec4`. When a symbol appears in the body of a shader that will be compiled and
does not resolve to anything in the current namespace, it is assumed to be a
GLSL builtin function or variable and will be interpolated into the resulting
source code without modification.

You can get the source of the shader from the resulting var by looking at the
`::sl/source` key.

``` clojure
user> (println (::sl/source vertex-shader))
#version 400 core






    out vec3 NS_user_SYM_geometry_color ;
  layout(location=1)  in vec3 NS_user_SYM_vertex_color ;
  layout(location=0)  in vec3 NS_user_SYM_vertex_position ;






void main()
{
gl_Position=vec4(NS_user_SYM_vertex_position, 1.0f);
NS_user_SYM_geometry_color=NS_user_SYM_vertex_color;
}

;; => nil
```

You can ignore the extraneous whitespace, it's used to separate several sections
which aren't included in this shader.

This example shows that the resulting identifiers are namespaced. This means you
can define parameters, shaders, shader functions, uniforms, etc., all in
different namespaces, and the shader will correctly identify them and include
them in the code, even if the unqualified names of the vars conflict.

In addition, it shows that the GLSL version and extensions are included at the
top of the shader file in the `#version` directive. You can change this to
support OpenGL ES or other versions by adding metadata to your var on the
`:version` and `:extensions` keys. The `:version` is a number, and `:extensions`
is a list of strings.

To complete this simple pipeline, a vertex shader is produced below.

``` clojure
(sl/defparam fragment-color "vec4"
  :layout {:location 0})

(sl/defshader fragment-shader
  {geometry-color :in
   fragment-color :out}
  (set! fragment-color (vec4 geometry-color (float 1))))
```

This simple shader passes the input color directly to the output without
modification, and sends this to the FBO color attachment 0.

In addition to parameters, uniforms are supported.

``` clojure
(sl/defuniform model-view-projection-matrix "mat4")

(sl/defshader camera-vertex-shader
  {vertex-position :in}
  (set! gl_Position (* model-view-projection-matrix (vec4 vertex-position (float 1)))))
```

When you set this uniform in the shader object, you can identify the uniform
name to OpenGL or other querying mechanisms by using `sl/sym->ident`.

``` clojure
user> (sl/sym->ident `model-view-projection-matrix)
;; => "NS_user_SYM_model_view_projection_matrix"
```

CLJSL also supports basic flow control operators, `if`, `cond`, `let`, `return`,
etc., as well as math operations, and n-ary comparators (where `(< a b c)`
compiles to `a < b && b < c`). Casting is also supported in the form `(cast
value type)`.

In addition, support for `for` loops is included, with the following syntax:

``` clojure
(for [x 10 (incf x)]
     (> x 0)
  (do stuff))
```

The operators `incf` and `decf` are provided for post-increment, and
post-decrement, respectively. Pre-* variants are provided with a `*`, i.e.
`incf*`.

Functions can be defined with `defshaderfn`, and must have type hinted arguments
and return.

``` clojure
(sl/defshaderfn square
  ^"float" [^"float" x]
  (return (* x x)))
```

No type-hinted return type is interpreted as a void return type, and explicit
returns via `return` must be used.

Structs can also be constructed with `defstruct`.

``` clojure
(sl/defstruct directional-light
  {:strength "float"
   :direction "vec3"})
```

This can be used with type hints.

``` clojure
(sl/defshaderfn calc-light
  ^"float" [^directional-light light ^"vec3" normal]
  (return (* (dot (- (:direction light)) normal) strength)))
```

Additionally, regular clojure macros can be used, as long as they produce valid
CLJSL code.

``` clojure
(defmacro square-macro-unsafe
  [x]
  `(* ~x ~x))
```

With the above macro, we can then compile the code using the `compile` function,
passing it the form to compile and a map including the local variables (in this
case just to prevent `compile` from attempting to resolve `x`).

``` clojure
user> (println (first (sl/compile '(square-macro-unsafe x) {'x 5})))
(x*x)
;; => nil
```

Since these macros are written in Clojure and not CLJSL, they have the full
power of the language and the normal language semantics.

In addition, there are several functions provided for compiling CLJSL code.
Their usage will not be explained here, but they are the basis on which the
macros work, and provide ways to build your own macros if their syntax or
semantics are unsatisfactory for your application.

## License

Copyright © 2021 Joshua Suskalo

Distributed under the Eclipse Public License version 1.0.
