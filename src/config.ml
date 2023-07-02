let verbose_names_ref = ref false
let verbose_names () = !verbose_names_ref
let set_verbose_names value = verbose_names_ref := value
let print_subst_unif_vars_ref = ref false
let print_subst_unif_vars () = !print_subst_unif_vars_ref
let set_print_subst_unif_vars value = print_subst_unif_vars_ref := value
let print_levels_ref = ref false
let print_levels () = !print_levels_ref
let set_print_levels value = print_levels_ref := value
