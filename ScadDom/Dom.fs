module ScadDom.Dom

open ScadDom.Common

type ScadDomNode =
    // Boolean Operations
    | Difference of children: ScadDomNode list
    | Union of children: ScadDomNode list
    | Intersection of children: ScadDomNode list

    // 2D Operations
    | Circle of radius: float
    | Square of width: float * height: float * center: bool
    | Polygon of points: Point2d list * paths: (int list) list option * convexity: int option
    // | Text of
    //     content: string
    //     * size: float option
    //     * font: string option
    //     * horizontalAlign: string option
    //     * verticalAlign: string option
    //     * spacing: float option
    //     * direction: string option
    //     * language: string option
    //     * script: string option
    | Import of filePath: string * convexity: int option * layer: string option
    | Projection of child: ScadDomNode * cut: bool

    // 3D Operations
    | Sphere of radius: float
    | Cube of width: float * depth: float * height: float * center: bool
    | Cylinder of height: float * radius1: float * radius2: float * center: bool
    // | Polyhedron of
    //     points: Point2d list
    //     * faces: (int list) list
    //     * convexity: int option
    | LinearExtrude of
        child: ScadDomNode *
        height: float *
        v: Point3d option *
        center: bool option *
        convexity: int option *
        twist: int option *
        slices: int option *
        scale: int option
    | RotateExtrude of child: ScadDomNode * convexity: int option * angle: float option * start: float option
    // | Surface of
    //     filePath: string
    //     * center: bool option
    //     * convexity: int option
    //     * invert: bool option

    // Transformations
    | Translate of child: ScadDomNode * vector: Point3d
    // | Rotate of
    //     child: ScadDomNode
    //     * rotationAnglesXYZ: Point2d option
    //     * angle: float option
    //     * axisVector: Point2d option
    | Scale of child: ScadDomNode * scaleVector: Point3d
    // | Resize of
    //     child: ScadDomNode
    //     * newDimensions: Point2d
    //     * autoX: bool
    //     * autoY: bool
    //     * autoZ: bool
    //     * convexity: int option
    | Mirror of child: ScadDomNode * mirrorVector: Point3d
    | MultMatrix of child: ScadDomNode * matrix: (float * float * float * float) list // Represents a 4x4 matrix
    | Color of ScadColor * child: ScadDomNode
    | OffsetRadius of child: ScadDomNode * r: float
    | OffsetDelta of child: ScadDomNode * delta: float * chamfer: bool
    | Hull of children: ScadDomNode list
    | Minkowski of convexity: int option * children: ScadDomNode list

    // Modifiers
    | AsteriskModifier of child: ScadDomNode
    | ExclamationModifier of child: ScadDomNode
    | HashModifier of child: ScadDomNode
    | PercentModifier of child: ScadDomNode
