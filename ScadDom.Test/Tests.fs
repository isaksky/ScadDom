module Tests

open System.Text.RegularExpressions
open ScadDom.Dom
open ScadDom.Scad
open ScadDom.Common

open Xunit

// User's ws, normalize, r, and @= definitions
let ws = Regex("\\s+")

let normalize (s: string) = ws.Replace(s, " ").Trim() // Added Trim() to handle potential leading/trailing spaces from normalization

let r dom =
    let s = renderToString dom
    normalize s

let inline (@=) (expected: string) (actual: ScadDomNode) =
    Assert.Equal((normalize expected), (r actual))

[<Fact>]
let ``Circle Works`` () = "circle(5);" @= Circle 5.0

[<Fact>]
let ``Square default center Works`` () =
    "square([10, 5], center = false);" @= Square(10.0, 5.0, false)

[<Fact>]
let ``Square centered Works`` () =
    "square([7, 7], center = true);" @= Square(7.0, 7.0, true)

[<Fact>]
let ``Sphere Works`` () = "sphere(10);" @= Sphere 10.0

[<Fact>]
let ``Cube single dimension Works`` () =
    "cube(5, center = false);" @= Cube(5.0, 5.0, 5.0, false)

[<Fact>]
let ``Cube multiple dimensions centered Works`` () =
    "cube([5, 6, 7], center = true);" @= Cube(5.0, 6.0, 7.0, true)

[<Fact>]
let ``Cylinder Works`` () =
    "cylinder(10, 3, 4, center = false);" @= Cylinder(10.0, 3.0, 4.0, false)

[<Fact>]
let ``Cylinder centered Works`` () =
    "cylinder(10, 5, 5, center = true);" @= Cylinder(10.0, 5.0, 5.0, true)

[<Fact>]
let ``Difference Works`` () =
    // Expected string is the target normalized output
    "difference() { circle(10); circle(5); }"
    @= Difference [ Circle 10.0; Circle 5.0 ]

[<Fact>]
let ``Union Works`` () =
    "union() { square([5, 5], center = false); circle(3); }"
    @= Union [ Square(5.0, 5.0, false); Circle 3.0 ]

[<Fact>]
let ``Intersection Works`` () =
    "intersection() { cube(10, center = false); sphere(7); }"
    @= Intersection [ Cube(10.0, 10.0, 10.0, false); Sphere 7.0 ]

[<Fact>]
let ``Translate Works`` () =
    "translate(v = [1, 2, 3]) { circle(5); }"
    @= Translate(Circle 5.0, { x = 1.0; y = 2.0; z = 3.0 })

[<Fact>]
let ``Scale Works`` () =
    "scale(v = [2, 0.5, 1]) { cube(5, center = false); }"
    @= Scale(Cube(5.0, 5.0, 5.0, false), { x = 2.0; y = 0.5; z = 1.0 })

[<Fact>]
let ``Mirror Works`` () =
    "mirror(v = [1, 0, 0]) { sphere(3); }"
    @= Mirror(Sphere 3.0, { x = 1.0; y = 0.0; z = 0.0 })

[<Fact>]
let ``Color named Works`` () =
    """color("Red") { circle(10); }""" @= Color(ScadColor.Red, Circle 10.0)

[<Fact>]
let ``Color RGBA Works`` () =
    """color(c = [0.1, 0.2, 0.3], alpha = 0.4) { square([5, 5], center = true); }"""
    @= Color(ScadColor(0.1, 0.2, 0.3, 0.4), Square(5.0, 5.0, true))

[<Fact>]
let ``Projection Works`` () =
    """projection(cut = true) { cube(5, center = false); }"""
    @= Projection(Cube(5.0, 5.0, 5.0, false), true)

[<Fact>]
let ``LinearExtrude minimal Works`` () =
    """linear_extrude(height = 10) { circle(5); }"""
    @= LinearExtrude(Circle 5.0, 10.0, None, None, None, None, None, None)

[<Fact>]
let ``LinearExtrude with options Works`` () =
    let le =
        LinearExtrude(
            child = Square(3.0, 3.0, true),
            height = 10.0,
            v = Some { x = 0.0; y = 0.0; z = 1.0 },
            center = Some true,
            convexity = Some 5,
            twist = Some 90,
            slices = Some 20,
            scale = Some 2
        )

    """linear_extrude(height = 10, v = [0, 0, 1], center = true, convexity = 5, twist = 90, slices = 20, scale = 2) { square([3, 3], center = true); }"""
    @= le

[<Fact>]
let ``RotateExtrude minimal Works`` () =
    """rotate_extrude() { circle(2); }"""
    @= RotateExtrude(Circle 2.0, None, None, None)

[<Fact>]
let ``RotateExtrude with options Works`` () =
    let re =
        RotateExtrude(child = Square(1.0, 4.0, false), convexity = Some 10, angle = Some 270.0, start = Some 45.0)
    """rotate_extrude(convexity = 10, angle = 270, start = 45) { square([1, 4], center = false); }"""
    @= re

[<Fact>]
let ``Import minimal Works`` () =
    """import("my_model.stl");""" @= Import("my_model.stl", None, None)

[<Fact>]
let ``Import with convexity Works`` () =
    """import("another.stl", convexity = 10);"""
    @= Import("another.stl", Some 10, None)

[<Fact>]
let ``Import with escaped characters in path Works`` () =
    let i = Import("path/with\"quotes\\and_slash.stl", Some 5, None)
    // Corrected F# string literal for the expected output:
    // Target OpenSCAD string content for path: path/with\"quotes\\and_slash.stl
    // F# literal for \": \\\"
    // F# literal for \\: \\\\\\\\
    "import(\"path/with\\\"quotes\\\\and_slash.stl\", convexity = 5);" @= i

[<Fact>]
let ``MultMatrix Works`` () =
    let mm =
        MultMatrix(
            child = Sphere 5.0,
            matrix =
                [ (1.0, 0.0, 0.0, 10.0)
                  (0.0, 1.0, 0.0, 20.0)
                  (0.0, 0.0, 1.0, 30.0)
                  (0.0, 0.0, 0.0, 1.0) ]
        )

    """multmatrix(m = [[1, 0, 0, 10],[0, 1, 0, 20],[0, 0, 1, 30],[0, 0, 0, 1]]) { sphere(5); }"""
    @= mm

[<Fact>]
let ``OffsetRadius Works`` () =
    """offset(r = 2.5) { circle(10); }""" @= OffsetRadius(Circle 10.0, 2.5)

[<Fact>]
let ``OffsetDelta Works`` () =
    let o = OffsetDelta(Square(5.0, 5.0, false), 1.0, true)
    """offset(delta = 1, chamfer = true) { square([5, 5], center = false); }""" @= o

[<Fact>]
let ``Hull Works`` () =
    let h = Hull [ Circle 5.0; Translate(Circle 5.0, { x = 10.0; y = 0.0; z = 0.0 }) ]
    """hull() { circle(5); translate(v = [10, 0, 0]) { circle(5); } }""" @= h

[<Fact>]
let ``Minkowski minimal Works`` () =
    let m = Minkowski(None, [ Sphere 5.0; Cube(1.0, 1.0, 1.0, true) ])
    """minkowski() { sphere(5); cube(1, center = true); }""" @= m

[<Fact>]
let ``Minkowski with convexity Works`` () =
    let m = Minkowski(Some 4, [ Sphere 5.0; Cube(1.0, 1.0, 1.0, true) ])
    """minkowski(4) { sphere(5); cube(1, center = true); }""" @= m

[<Fact>]
let ``AsteriskModifier Works`` () =
    "*circle(5);" @= AsteriskModifier(Circle 5.0)

[<Fact>]
let ``ExclamationModifier Works`` () =
    "!sphere(2);" @= ExclamationModifier(Sphere 2.0)

[<Fact>]
let ``HashModifier Works`` () =
    "#cube(3, center = false);" @= HashModifier(Cube(3.0, 3.0, 3.0, false))

[<Fact>]
let ``PercentModifier Works`` () =
    "%cylinder(5, 1, 1, center = true);"
    @= PercentModifier(Cylinder(5.0, 1.0, 1.0, true))

[<Fact>]
let ``Nested modifiers and operations Works`` () =
    let n =
        Union
            [ AsteriskModifier(Circle 10.0)
              Translate(HashModifier(Cube(5.0, 5.0, 5.0, true)), { x = 10.0; y = 0.0; z = 0.0 }) ]
    // Adopting the compact modifier style and semicolons for children
    """union() { *circle(10); translate(v = [10, 0, 0]) { #cube(5, center = true); } }"""
    @= n
