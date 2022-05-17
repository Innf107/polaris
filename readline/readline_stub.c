#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>

CAMLprim value readline_stub(value prompt) {
    using_history();

    char* result_str = readline(String_val(prompt));

    if (result_str == NULL){
        return Val_none;
    } else {
        add_history(result_str);
        value result = caml_copy_string(result_str);
        free(result_str);
        return caml_alloc_some(result);
    }
}
