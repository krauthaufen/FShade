module FShade.Aot.Tests.Shaders.Definitions

open Aardvark.Base
open FShade

type Vertex =
    {
        [<Position>] pos : V4d
        [<Color>]    col : V4d
    }

/// constant-color frag shader (parametrised by color).
let frag_constantColor (color : V4d) (v : Vertex) =
    fragment {
        return color * v.col
    }

/// scaled by an arbitrary factor.
let frag_scaled (a : V4d) (b : float) (v : Vertex) =
    fragment {
        return v.col * (a * b)
    }

/// vertex transform (single scale factor).
let vert_scale (s : float) (v : Vertex) =
    vertex {
        return { v with pos = v.pos * s }
    }

/// many curried args to test FSharpFunc chaining.
let frag_many (a : float) (b : float) (c : float) (d : float) (e : float) (f : float) (v : Vertex) =
    fragment {
        return v.col * (a + b + c + d + e + f)
    }

/// integer arg + name shadowing builtin.
let frag_int_arg (count : int) (v : Vertex) =
    fragment {
        return v.col * float count
    }

/// Returns a closure that captures an outer value. The closure's Invoke
/// method is the actual shader function — instance method on the F# closure type.
let makeCapturedFrag (outerScale : float) =
    fun (color : V4d) (v : Vertex) ->
        fragment {
            return color * outerScale * v.col
        }

/// Mirrors Aardvark.Rendering's DefaultSurfaces.constantColor: parameter is rebound
/// before entering the CE block. The rebound `c` shadows the parameter `c` and has a
/// different type — exercises the carrier-normalization edge case.
let constantColorLike (c : C4b) (v : Vertex) =
    let c = c.ToV4d()
    fragment {
        return c * v.col
    }

/// ZERO-arg shader (only the input). The marker-precomputed path applies — fshadeaot
/// fully runs Shader.ofExpr at build time and embeds the serialized result.
let frag_passthrough (v : Vertex) =
    fragment {
        return v.col
    }

/// Another zero-arg shader, slightly more interesting body.
let frag_swizzle (v : Vertex) =
    fragment {
        return V4d(v.col.W, v.col.X, v.col.Y, v.col.Z)
    }

// ---- closure cases (instance shader functions) ----

/// Genuine closure: factory has a side effect so F# can't flatten.
/// The result is a closure type with a captured `outerScale` field; its Invoke
/// method is the actual shader function — instance method, not static.
let makeClosureSideEffect (outerScale : float) =
    System.Console.WriteLine ""   // prevents F# eta-flattening
    fun (v : Vertex) ->
        fragment { return v.col * outerScale }
let closure_with_capture = makeClosureSideEffect 2.5

/// Nested closure: outer captures `a`, inner captures `a` and `c`.
let makeNestedClosure (a : float) =
    System.Console.WriteLine ""
    fun (c : V4d) ->
        System.Console.WriteLine ""
        fun (v : Vertex) ->
            fragment { return v.col * a + c }
let nested_closure_partial = makeNestedClosure 1.5
let nested_closure = nested_closure_partial (V4d(0.1, 0.2, 0.3, 1.0))
