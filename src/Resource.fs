module Resource

open System.Drawing
open System.Reflection
open System.Resources
open System.Windows.Forms


let executingAssembly = Assembly.GetExecutingAssembly()
let resources = new ResourceManager("Resources", executingAssembly)

let string = resources.GetString

let image name = resources.GetObject(name) :?> Image

let title = string "title"

let t = [|image "t1"; image "t2"; image "t3"; image "t4"|]

let o = [|image "o1"; image "o2"; image "o3"; image "o4"|]

let l = [|image "l1"; image "l2"; image "l3"; image "l4"|]

let i = [|image "i1"; image "i2"; image "i3"; image "i4"|]

let j = [|image "j1"; image "j2"; image "j3"; image "j4"|]

let z = [|image "z1"; image "z2"; image "z3"; image "z4"|]

let s = [|image "s1"; image "s2"; image "s3"; image "s4"|]