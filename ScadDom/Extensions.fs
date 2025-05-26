namespace ScadDom

open System.Runtime.CompilerServices
open ScadDom.Dom

[<Extension>]
type Extensions =
    //=========================================================================
    // Transformations
    //=========================================================================

    /// <summary>Creates a Translate node.</summary>
    [<Extension>]
    static member Translate(child: ScadDomNode, vector: Point3d) = Factory.Translate(vector, child)

    /// <summary>Creates a Scale node.</summary>
    [<Extension>]
    static member Scale(child: ScadDomNode, scaleVector: Point3d) = Factory.Scale(scaleVector, child)

    /// <summary>Creates a Mirror node.</summary>
    [<Extension>]
    static member Mirror(child: ScadDomNode, mirrorVector: Point3d) = Factory.Mirror(mirrorVector, child)

    /// <summary>Creates a Color node.</summary>
    [<Extension>]
    static member Color(child: ScadDomNode, color: ScadColor) = Factory.Color(color, child)

    /// <summary>Creates an Offset node with a radius.</summary>
    [<Extension>]
    static member Offset(child: ScadDomNode, radius: float) = Factory.Offset(radius, child)

    /// <summary>Creates an Offset node with a delta.</summary>
    [<Extension>]
    static member Offset(
            child: ScadDomNode,
            delta: float,
            [<System.Runtime.InteropServices.Optional;
              System.Runtime.InteropServices.DefaultParameterValue(false)>] chamfer:bool
        ) =
        Factory.Offset(delta, child, chamfer)

    //=========================================================================
    // Modifiers
    //=========================================================================

    /// <summary>Simply ignore this entire subtree.</summary>
    [<Extension>]
    static member Disable(child: ScadDomNode) = Factory.Disable(child)

    /// <summary>Use this subtree as usual in the rendering process but also draw it unmodified in transparent pink.</summary>
    [<Extension>]
    static member Debug(child: ScadDomNode) = Factory.Debug(child)

    /// <summary>Ignore this subtree for the normal rendering process and draw it in transparent gray.</summary>
    [<Extension>]
    static member Background(child: ScadDomNode) = Factory.Background(child)

    /// <summary>Ignore the rest of the design and use this subtree as design root.</summary>
    [<Extension>]
    static member Root(child: ScadDomNode) = Factory.Root(child)
