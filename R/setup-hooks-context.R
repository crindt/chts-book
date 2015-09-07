source(pp("R/setup-hooks.R"))

# some LaTeX (pdf) specific formatting
knit_hooks$set(output = function(x, options) {
    paste("\n\n\\begin{OutputShaded}\n\\begin{Verbatim}[frame=leftline,framerule=0.75mm]\n", x, "\\end{Verbatim}\n\\end{OutputShaded}", sep = "")
}, error = function(x, options) {
    paste("\n\n\\begin{ErrorShaded}\n\\begin{Verbatim}[frame=leftline,framerule=0.75mm]\n", x, "\\end{Verbatim}\n\\end{ErrorShaded}", sep = "")
})
