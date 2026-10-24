long_a <- "an_extremely_long_column_name_used_for_label_tests_aaaaaaaaaaaaaaaaaaaa" # 70 characters
block <- quote({
  x
})

# Symbols and constants -------------------------------------------------------

expect_identical(poorman:::auto_label(quote(weight)), "weight", info = "auto_label() names a symbol after itself")
expect_identical(
  poorman:::auto_label(quote(`my col`)),
  "my col",
  info = "auto_label() drops the backticks of a lone symbol"
)
expect_identical(poorman:::auto_label(as.name(long_a)), long_a, info = "auto_label() never shortens a lone symbol")
expect_identical(poorman:::auto_label(quote(.data$wt)), "wt", info = "auto_label() names .data$x after the column")
expect_identical(
  poorman:::auto_label(quote(.data[["wt"]])),
  "wt",
  info = "auto_label() names .data[[\"x\"]] after the column"
)
expect_identical(
  poorman:::auto_label(quote(sum(.data$wt))),
  "sum(.data$wt)",
  info = "auto_label() keeps .data inside a call"
)
expect_identical(poorman:::auto_label(1e5), "1e+05", info = "auto_label() deparses a number")
expect_identical(
  poorman:::auto_label("a long string that is never shortened when it is on its own, however long it gets"),
  "\"a long string that is never shortened when it is on its own, however long it gets\"",
  info = "auto_label() never shortens a lone string"
)

# Binary operators ------------------------------------------------------------

expect_identical(
  poorman:::auto_label(quote(mean(x) + 1)),
  "mean(x) + 1",
  info = "auto_label() keeps a short operator call"
)
expect_identical(
  poorman:::auto_label(quote(x %% 2 == 0)),
  "x%%2 == 0",
  info = "auto_label() keeps R's spacing in a short call"
)
expect_identical(
  poorman:::auto_label(quote(
    first_measure_column + second_measure_column + third_measure_column + fourth_measure_column
  )),
  "... + fourth_measure_column",
  info = "auto_label() hides the left chain of a long operator call"
)
expect_identical(
  poorman:::auto_label(quote(
    first_measure_column_name_abc + (second_measure_column_name + third_measure_column_name)
  )),
  "first_measure_column_name_abc + ...",
  info = "auto_label() keeps the left operand when it fits"
)
expect_identical(
  poorman:::auto_label(quote(
    (alpha_beta_gamma_delta + epsilon_zeta_eta_theta + iota_kappa_lambda) * mu_nu_xi_omicron
  )),
  "... * mu_nu_xi_omicron",
  info = "auto_label() keeps the right operand when only it fits"
)
expect_identical(
  poorman:::auto_label(call("+", as.name(long_a), as.name(long_a))),
  "... + ...",
  info = "auto_label() hides both operands when neither fits"
)
expect_identical(
  poorman:::auto_label(call("/", as.name(long_a), quote(short_name))),
  "... / short_name",
  info = "auto_label() spaces / in a shortened label (agreed difference D1 from R's own printing)"
)
expect_identical(
  poorman:::auto_label(call("%%", quote(left_operand_name), as.name(long_a))),
  "left_operand_name %% ...",
  info = "auto_label() spaces %% in a shortened label (agreed difference D1)"
)
expect_identical(
  poorman:::auto_label(call("+", quote((a / b)), as.name(long_a))),
  "(a / b) + ...",
  info = "auto_label() spaces / inside a kept operand (agreed difference D1)"
)
expect_identical(
  poorman:::auto_label(call("^", as.name(long_a), quote(y))),
  "...^y",
  info = "auto_label() puts no spaces around ^ in a shortened label"
)
expect_identical(
  poorman:::auto_label(call("$", as.name(long_a), quote(y))),
  "...$y",
  info = "auto_label() puts no spaces around $ in a shortened label"
)
expect_identical(
  poorman:::auto_label(call("+", as.name(strrep("v", 57L)), quote(y))),
  "... + y",
  info = "auto_label() shortens an operator call of 61 characters"
)
expect_identical(
  poorman:::auto_label(call("+", as.name(strrep("v", 54L)), quote(g()))),
  paste0(strrep("v", 54L), " + g()"),
  info = "auto_label() keeps an operator call of exactly 60 characters"
)
expect_identical(
  poorman:::auto_label(call("+", as.name(strrep("v", 55L)), quote(g()))),
  "... + g()",
  info = "auto_label() uses a fixed limit of 60 characters (deliberate difference D2: dplyr 1.0.10 keeps this label)"
)
expect_identical(
  poorman:::auto_label(call(":=", as.name(long_a), quote(y))),
  "`:=`(...)",
  info = "auto_label() treats := as a prefix call (deliberate difference D3 from dplyr 1.0.10)"
)
expect_identical(
  poorman:::auto_label(call("?", as.name(long_a), quote(y))),
  "`?`(...)",
  info = "auto_label() treats ? as a prefix call (deliberate difference D3 from dplyr 1.0.10)"
)
# Outside a UTF-8 locale, R escapes the characters and the line is longer
if (l10n_info()[["UTF-8"]]) {
  wide <- call("+", as.name(paste0("a", strrep("é", 50L))), quote(y)) # at most 57 characters, over 100 bytes
  expect_identical(
    poorman:::auto_label(wide),
    paste(deparse(wide, width.cutoff = 500L), collapse = " "),
    info = "auto_label() counts characters, not bytes (deliberate difference from dplyr 1.0.10, which gives `+...`)"
  )
  rm(wide)
}
expect_identical(poorman:::auto_label(call("+", block, 1)), "... + 1", info = "auto_label() hides a block operand")

# Other calls -----------------------------------------------------------------

expect_identical(
  poorman:::auto_label(quote(a_long_function_name(first_argument_value, second_argument_value, third_argument = 3))),
  "a_long_function_name(...)",
  info = "auto_label() shortens a call that R wraps"
)
expect_identical(
  poorman:::auto_label(quote(
    pkg::a_long_function_name(first_argument_value, second_argument_value, third_argument = 3)
  )),
  "pkg::a_long_function_name(...)",
  info = "auto_label() keeps the namespace of a shortened call"
)
expect_identical(
  poorman:::auto_label(call("f", as.name(long_a))),
  paste0("f(", long_a, ")"),
  info = "auto_label() keeps a long call that R does not wrap"
)
expect_identical(
  poorman:::auto_label(call(
    "-",
    quote(a_long_function_name(first_argument_value, second_argument_value, third_argument = 3))
  )),
  "-...",
  info = "auto_label() shortens a wrapped unary operator call"
)
expect_identical(
  poorman:::auto_label(call(
    "(",
    quote(a_long_function_name(first_argument_value, second_argument_value, third_argument = 3))
  )),
  "(...)",
  info = "auto_label() shortens wrapped brackets"
)
expect_identical(
  poorman:::auto_label(quote(
    first_measure_column[second_measure_column > 1 & third_measure_column < 2 & fourth_col > 3, ]
  )),
  "...[]",
  info = "auto_label() shortens a wrapped [ call"
)
expect_identical(
  poorman:::auto_label(quote(
    first_measure_column[[second_measure_column + third_measure_column + fourth_measure_column]]
  )),
  "...[[]]",
  info = "auto_label() shortens a wrapped [[ call"
)
expect_identical(poorman:::auto_label(block), "{ ... }", info = "auto_label() always shortens a block")
expect_identical(
  poorman:::auto_label(call("function", as.pairlist(alist(x = )), block)),
  "function(x) ...",
  info = "auto_label() hides the body of a wrapped function"
)
expect_identical(
  poorman:::auto_label(quote(
    function(first_arg = 1, second_arg = 2, third_arg = 3, fourth_arg = 4, fifth_arg = 5) NULL
  )),
  "function(first_arg = 1, second_arg = 2, third_arg = 3, fourth_arg = 4, ...",
  info = "auto_label() keeps the first line of wrapped function formals"
)
expect_identical(
  poorman:::auto_label(call(
    "if",
    quote(first_measure_column > second_measure_column & third_measure_column),
    block,
    quote(other)
  )),
  "if (...) NULL",
  info = "auto_label() shortens a wrapped if"
)

rm(long_a, block)
