module AST = struct
  type le = Le of char
  type cc = CC of char
  type di = Di of int
  type bc = BC of bool

  type id =
    | Id_Na of na
    | Id_DotNa of id * na
    | Id_Brac of id * e
    | Id_Deref of id
    | Id_AddrOf of id

  and c =
    | C_DiS of dis (* Signed integer *)
    | C_DiSu of dis (* Unsigned integer *)
    | C_Null

  and na =
    | Na_Single of le
    | Na_Seq of le * diles

  and dile =
    | DiLe_Le of le
    | DiLe_Di of di

  and diles =
    | DiLeS_Single of dile
    | DiLeS_Seq of dile * diles

  and dis =
    | DiS_Single of di
    | DiS_Seq of di * dis

  and f =
    | F_Id of id
    | F_Neg of f
    | F_Paren of e
    | F_C of c

  and t =
    | T_F of f
    | T_Mul of t * f
    | T_Div of t * f

  and e =
    | E_T of t
    | E_Add of e * t
    | E_Sub of e * t

  and atom =
    | Atom_Greater of e * e
    | Atom_GreaterEq of e * e
    | Atom_Less of e * e
    | Atom_LessEq of e * e
    | Atom_Eq of e * e
    | Atom_NotEq of e * e
    | Atom_BC of bc

  and be =
    | BE_BT of bt
    | BE_Or of be * bt

  and bt =
    | BT_BF of bf
    | BT_And of bt * bf

  and bf =
    | BF_Id of id
    | BF_Atom of atom
    | BF_Not of bf
    | BF_Paren of be

  and pa =
    | Pa_E of e
    | Pa_BE of be
    | Pa_CC of cc

  and pas =
    | PaS_Single of pa
    | PaS_Seq of pa * pas  

  and rst =
    | RSt_E of e
    | RSt_BE of be
    | RSt_CC of cc

  and st =
    | St_EAssign of id * e
    | St_BEAssign of id * be
    | St_CCAssign of id * cc

    | St_If of be * sts
    | St_IfElse of be * sts * sts
    | St_While of be * sts

    | St_CallAssign of id * na * pas option
    | St_PtrAssign of id * na

  and sts =
    | StS_Single of st
    | StS_Seq of st * sts

  and body =
    | Body of sts option * rst

  and pads =
    | PaDS_Single of vad
    | PaDS_Seq of vad * pads

  and fud =
    | FuD of ty * na * pads option * vads option * body

  and fuds =
    | FuDS_Sigle of fud
    | FuDS_Seq of fud * fuds
  
  and vad =
    | Vad of ty * na

  and vads =
    | VaDS_Sigle of vad
    | VaDS_Seq of vad * vads

  and ty =
    | Ty_Int
    | Ty_Bool
    | Ty_Char
    | Ty_UInt
    | Ty_Na of na

  and te =
    | TE_Arr of ty * dis
    | TE_Ptr of ty
    | TE_Struct of vads

  and tyd =
    | TyD of te * na

  and tyds =
    | TyDS_Single of tyd
    | TyDS_Seq of tyd * tyds

  and prog =
    | Prog of tyds option * vads option * fuds
end
