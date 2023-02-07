
let verbose_names_ref = ref false

let verbose_names () = !verbose_names_ref
let set_verbose_names value = verbose_names_ref := value
