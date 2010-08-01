open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
  | After_rules ->
      flag ["compile"; "ocaml"; "use_gtk";]
          (S[A"-I"; A"+lablgtk2";]);
      flag ["link"; "ocaml"; "use_gtk"; "byte";]
          (S[A"-I"; A"+lablgtk2"; A"lablgtk.cma"; A"gtkInit.cmo";]);
      flag ["link"; "ocaml"; "use_gtk"; "native";]
          (S[A"-I"; A"+lablgtk2"; A"lablgtk.cmxa"; A"gtkInit.cmx";]);
  | _ -> ()
end;;





