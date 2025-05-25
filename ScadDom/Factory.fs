namespace ScadDom

open System
open System.Collections.Generic
open ScadDom.Common
open System.Runtime.InteropServices
open ScadDom.Dom

/// <summary>
/// A static factory class for SCAD Dom nodes.
/// </summary>
type Factory =
    private new() = {}

    //=========================================================================
    // Boolean Operations
    //=========================================================================

    /// <summary>Creates a Difference node from a sequence of children.</summary>
    static member Difference(children: IEnumerable<ScadDomNode>) =
        Difference(List.ofSeq children)

    /// <summary>Creates a Difference node from a parameter array of children.</summary>
    static member Difference([<ParamArray>] children: ScadDomNode[]) =
        Difference(List.ofArray children)

    /// <summary>Creates a Union node from a sequence of children.</summary>
    static member Union(children: IEnumerable<ScadDomNode>) =
        Union(List.ofSeq children)

    /// <summary>Creates a Union node from a parameter array of children.</summary>
    static member Union([<ParamArray>] children: ScadDomNode[]) =
        Union(List.ofArray children)

    /// <summary>Creates an Intersection node from a sequence of children.</summary>
    static member Intersection(children: IEnumerable<ScadDomNode>) =
        Intersection(List.ofSeq children)

    /// <summary>Creates an Intersection node from a parameter array of children.</summary>
    static member Intersection([<ParamArray>] children: ScadDomNode[]) =
        Intersection(List.ofArray children)

    //=========================================================================
    // 2D Operations
    //=========================================================================

    /// <summary>Creates a Circle node.</summary>
    static member Circle(radius: float) = Circle radius

    /// <summary>Creates a Square node.</summary>
    static member Square(width: float, height: float, [<Optional; DefaultParameterValue(false)>] center: bool) =
        Square(width, height, center)

    /// <summary>Creates a uniform Square node where width equals height.</summary>
    static member Square(size: float, [<Optional; DefaultParameterValue(false)>] center: bool) =
        Square(size, size, center)

    /// <summary>Creates a Polygon node.</summary>
    static member Polygon(
            points: IEnumerable<Point2d>,
            [<Optional; DefaultParameterValue(null: IEnumerable<IEnumerable<int>>)>] paths:
                IEnumerable<IEnumerable<int>>,
            [<Optional; DefaultParameterValue(Nullable<int>())>] convexity: Nullable<int>
        ) =
        let pointsList = List.ofSeq points

        let pathsOpt =
            paths |> Option.ofObj |> Option.map (Seq.map List.ofSeq >> List.ofSeq)

        let convexityOpt = Option.ofNullable convexity
        Polygon(pointsList, pathsOpt, convexityOpt)

    /// <summary>Creates an Import node.</summary>
    static member Import(
            filePath: string,
            [<Optional; DefaultParameterValue(Nullable<int>())>] convexity: Nullable<int>,
            [<Optional; DefaultParameterValue(null: string)>] layer: string
        ) =
        Import(filePath, Option.ofNullable convexity, Option.ofObj layer)

    /// <summary>Creates a Projection node.</summary>
    static member Projection(child: ScadDomNode, [<Optional; DefaultParameterValue(false)>] cut: bool) =
        Projection(child, cut)

    //=========================================================================
    // 3D Operations
    //=========================================================================

    /// <summary>Creates a Sphere node.</summary>
    static member Sphere(radius: float) = Sphere radius

    /// <summary>Creates a Cube node with specified dimensions.</summary>
    static member Cube(
        width: float, depth: float, height: float,
        [<Optional; DefaultParameterValue(false)>] center: bool
    )
        =
        Cube(width, depth, height, center)

    /// <summary>Creates a uniform Cube node where all sides are equal.</summary>
    static member Cube(size: float, [<Optional; DefaultParameterValue(false)>] center: bool) =
        Cube(size, size, size, center)

    /// <summary>Creates a Cylinder node.</summary>
    static member Cylinder
        (height: float, radius1: float, radius2: float, [<Optional; DefaultParameterValue(false)>] center: bool) =
        Cylinder(height, radius1, radius2, center)

    /// <summary>Creates a Cylinder node with a single radius (r1 = r2).</summary>
    static member Cylinder(height: float, radius: float, [<Optional; DefaultParameterValue(false)>] center: bool) =
        Cylinder(height, radius, radius, center)

    /// <summary>Creates a LinearExtrude node.</summary>
    static member LinearExtrude(
            child: ScadDomNode,
            height: float,
            [<Optional; DefaultParameterValue(Nullable<Point3d>())>] v: Nullable<Point3d>,
            [<Optional; DefaultParameterValue(Nullable<bool>())>] center: Nullable<bool>,
            [<Optional; DefaultParameterValue(Nullable<int>())>] convexity: Nullable<int>,
            [<Optional; DefaultParameterValue(Nullable<int>())>] twist: Nullable<int>,
            [<Optional; DefaultParameterValue(Nullable<int>())>] slices: Nullable<int>,
            [<Optional; DefaultParameterValue(Nullable<int>())>] scale: Nullable<int>
        ) =
        LinearExtrude(
            child,
            height,
            Option.ofNullable v,
            Option.ofNullable center,
            Option.ofNullable convexity,
            Option.ofNullable twist,
            Option.ofNullable slices,
            Option.ofNullable scale
        )

    /// <summary>Creates a RotateExtrude node.</summary>
    static member RotateExtrude(
            child: ScadDomNode,
            [<Optional; DefaultParameterValue(Nullable<int>(): Nullable<int>)>] convexity: Nullable<int>,
            [<Optional; DefaultParameterValue(Nullable<float>(): Nullable<float>)>] angle: Nullable<float>,
            [<Optional; DefaultParameterValue(Nullable<float>(): Nullable<float>)>] start: Nullable<float>
        ) =
        RotateExtrude(child, Option.ofNullable convexity, Option.ofNullable angle, Option.ofNullable start)

    //=========================================================================
    // Transformations
    //=========================================================================

    /// <summary>Creates a Translate node.</summary>
    static member Translate(vector: Point3d, child: ScadDomNode) = Translate(child, vector)

    /// <summary>Creates a Scale node.</summary>
    static member Scale(scaleVector: Point3d, child: ScadDomNode) = Scale(child, scaleVector)

    /// <summary>Creates a Mirror node.</summary>
    static member Mirror(mirrorVector: Point3d, child: ScadDomNode) = Mirror(child, mirrorVector)

    /// <summary>Creates a MultMatrix node from a sequence of 4-element tuples.</summary>
    static member MultMatrix(matrix: IEnumerable<(float * float * float * float)>, child: ScadDomNode) =
        MultMatrix(child, List.ofSeq matrix)

    /// <summary>Creates a Color node.</summary>
    static member Color(color: ScadColor, child: ScadDomNode) = Color(color, child)

    /// <summary>Creates an Offset node with a radius.</summary>
    static member Offset(radius: float, child: ScadDomNode) = OffsetRadius(child, radius)

    /// <summary>Creates an Offset node with a delta.</summary>
    static member Offset(delta: float, child: ScadDomNode, [<Optional; DefaultParameterValue(false)>] chamfer: bool) =
        OffsetDelta(child, delta, chamfer)

    /// <summary>Creates a Hull node from a sequence of children.</summary>
    static member Hull(children: IEnumerable<ScadDomNode>) = Hull(List.ofSeq children)

    /// <summary>Creates a Hull node from a parameter array of children.</summary>
    static member Hull([<ParamArray>] children: ScadDomNode[]) = Hull(List.ofArray children)

    /// <summary>Creates a Minkowski node from a sequence of children.</summary>
    static member Minkowski(
            children: IEnumerable<ScadDomNode>,
            [<Optional; DefaultParameterValue(Nullable<int>())>] convexity: Nullable<int>
        ) =
        Minkowski(Option.ofNullable convexity, List.ofSeq children)

    /// <summary>Creates a Minkowski node from a parameter array of children.</summary>
    static member Minkowski(
            [<Optional; DefaultParameterValue(Nullable<int>())>] convexity: Nullable<int>,
            [<ParamArray>] children: ScadDomNode[]
        ) =
        Minkowski(Option.ofNullable convexity, List.ofArray children)

    //=========================================================================
    // Modifiers
    //=========================================================================

    /// <summary>Simply ignore this entire subtree.</summary>
    static member Disable(child: ScadDomNode) = DisableModifier child

    /// <summary>Use this subtree as usual in the rendering process but also draw it unmodified in transparent pink.</summary>
    static member Debug(child: ScadDomNode) = DebugModifier child

    /// <summary>Ignore this subtree for the normal rendering process and draw it in transparent gray (all
    /// transformations are still applied to the nodes in this tree).
    ///
    /// Because the marked subtree is completely ignored, it might have unexpected effects in case it's used, for
    /// example, with the first object in a difference(). In that case this object is rendered in transparent gray,
    /// but it is not used as the base for the difference()!</summary>
    static member Background(child: ScadDomNode) = BackgroundModifier child

    /// <summary>Ignore the rest of the design and use this subtree as design root.</summary>
    static member Root(child: ScadDomNode) = RootModifier child

