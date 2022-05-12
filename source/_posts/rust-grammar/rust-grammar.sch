;; -*- mode: scheme; indent-tabs-mode: nil -*-

(let* ((file (current-source-file))
       (path-parent (lambda (x)
                      (list->string (reverse (memv #\/ (cdr (reverse (string->list x))))))))
       (parent-dir (path-parent (path-parent (symbol->string file)))))
  (parameterize ((current-require-path (cons (string-append parent-dir "earley-hacks")
                                             (current-require-path))))
    (require 'earley)))


(define ebnf-grammar-from-rust.md-doc
  '((grammar        -> rule + )
    (rule           -> nonterminal ":" productionrule ";" )
    (productionrule -> production [ "|" production ] * )
    ;; repeating above with '!' as '|' alternative; latter not legal in Larceny
    (productionrule -> production [ "!" production ] * )
    (production     -> term * )
    (term           -> element repeats )
    (element        -> LITERAL ! IDENTIFIER ! "[" productionrule "]" )
    (repeats        -> [ "*" ! "+" ] NUMBER ? ! NUMBER ? ! "?" )))

;; LITERAL is single printable ASCII character.  Raw EBNF wraps in
;; single-quotes (for both single letter and \xQQ cases); Larceny
;; input can use analogous character input syntax #\name and #\xQQ.

;; IDENTIFIER is nonempty string of ASCII letters and underscores.

(define rust-grammar-from-rust.md-doc
  '(
    (ident              -> XID_start XID_continue * ) ;; *minus* set of keywords.  TODO

    (non_null           -> Any_aside_from_x0000     )
    (non_eol            -> Any_aside_from_x000A     )
    (non_star           -> Any_aside_from_x002A     )
    (non_slash          -> Any_aside_from_x002F     )
    (non_single_quote   -> Any_aside_from_x0027     )
    (non_double_quote   -> Any_aside_from_x0022     )

    (comment -> block_comment)
    (comment -> line_comment)

    (block_comment      -> "/*" block_comment_body * "*/" )
    (block_comment_body -> non_star * )
    (block_comment_body -> '*' non_slash )
    (line_comment       -> "//" non_eol * )

    (whitespace_char    -> #\x20 ! #\x09 ! #x0a ! #x0d     )
    (whitespace         -> [ whitespace_char ! comment ] + )

    (simple_token       -> keyword ! unop ! binop      )
    (token              -> simple_token ! ident ! literal ! symbol ! whitespace token )

    (keyword            -> "as" ! "assert" ! "break" ! "const" ! "copy"
                         ! "do" ! "drop" ! "else" ! "enum" ! "extern"
                         ! "false" ! "fn" ! "for" ! "if" ! "impl"
                         ! "let" ! "log" ! "loop"
                         ! "match" ! "mod" ! "move" ! "mut"
                         ! "priv" ! "pub" ! "pure" ! "ref" ! "return"
                         ! "self" ! "static" ! "struct" ! "super"
                         ! "true" ! "trait" ! "type"
                         ! "unsafe" ! "use" ! "while" )

    (literal            -> string_lit ! char_lit ! num_lit )

    (char_lit           -> #\' char_body   #\' )
    (string_lit         -> #\" string_body #\" )
    (char_body          -> non_single_quote ! #\\ [ #\' ! common_escape ] )
    (string_body        -> non_double_quote ! #\\ [ #\" ! common_escape ] )
    (common_escape      -> #\\
                         ! #\n ! #\r ! #\t
                         ! #\x hex_digit 2
                         ! #\u hex_digit 4
                         ! #\U hex_digit 8 )
    (hex_digit          -> #\a ! #\b ! #\c ! #\d ! #\e ! #\f
                         ! #\A ! #\B ! #\C ! #\D ! #\E ! #\F
                         ! dec_digit )
    (dec_digit          -> #\0 ! nonzero_dec )
    (nonzero_dec        -> #\1 ! #\2 ! #\3 ! #\4
                         ! #\5 ! #\6 ! #\7 ! #\8 ! #\9)

    (num_lit            -> nonzero_dec [ dec_digit ! #\_ ] * num_suffix ?
                         ! #\0 [       [ dec_digit ! #\_ ] + num_suffix ?
                               ! #\b   [ #\1 ! #\0 ! #\_ ] + int_suffix ?
                               ! #\x   [ hex_digit ! #\_ ] + int_suffix ? ] )
    (num_suffix         -> int_suffix ! float_suffix )
    (int_suffix         -> #\u int_suffix_size ?
                         ! #\i int_suffix_size )
    (float_suffix       -> [ exponent ! #\. dec_lit exponent ? ] float_suffix_ty ? )
    (float_suffix_ty    -> #\f [ #\3 #\2 ! #\6 #\4 ])
    (exponent           -> [ #\E ! #\e ] [ #\- ! #\+ ] ? dec_lit )
    (dec_lit            -> [ dec_digit ! #\_ ] + )

    ))

(define rust-grammar-from-parser-source-code
  '(( <crate_mod>                   -> <crate_attrs> <mod_items> )
    ( <crate_attrs>                 -> <inner_attrs_and_next> )

    ( <outer_attributes>            -> <attribute-outer> <outer_attributes> )
    ( <outer_attributes>            -> <doc_comment-style-outer> <outer_attributes> )
    ( <outer_attributes>            -> )

    ( <attribute-outer>             -> "#" <attribute_naked-outer> )
    ( <attribute-inner>             -> "#" <attribute_naked-inner> )
    ( <attribute_naked-outer>       -> "[" <meta_item> "]"         )
    ( <attribute_naked-inner>       -> "[" <meta_item> "]"         )

    ( <inner_attrs_and_next>        -> <attribute-inner> ";" <inner_attrs_and_next> )
    ( <inner_attrs_and_next>        -> <attribute-inner> ) ;; no ";" means not really inner_attr; outer_attr
    ( <inner_attrs_and_next>        -> <doc_comment-style-inner> <inner_attrs_and_next> )
    ( <inner_attrs_and_next>        -> <doc_comment-style-outer> )
    ( <inner_attrs_and_next>        -> )

    ( <meta_item>                   -> <ident> "=" <lit> )
    ( <meta_item>                   -> <ident> "(" <meta_seq> ")" )
    ( <meta_item>                   -> <ident> )

    ( <meta_seq>                    -> <meta-item> )
    ( <meta_seq>                    -> <meta-item> "," <meta_seq> )

    ( <ty_bare_fn>                  -> <purity> "fn" <ty_fn_decl> )

    ( <ty_closure>                  -> <purity> <onceness> "fn" <fn_ty_sigil> <ty_fn_decl> )

    ( <onceness>                    -> "once" )
    ( <onceness>                    ->        )

    ( <purity>                      -> "pure"   )
    ( <purity>                      -> "unsafe" )
    ( <purity>                      ->          )

    ( <fn_ty_decl>                  -> "(" <seq-comma-arg_general-false> ")" <ret_ty> )

    ( <seq-comma-arg_general-false>   -> )
    ( <seq-comma-arg_general-false>   -> <neseq-comma-arg_general-false> )
    ( <neseq-comma-arg_general-false> -> <arg_general-false> "," <neseq-comma-arg_general-false> )
    ( <neseq-comma-arg_general-false> -> <arg_general-false> )

    ( <trait_methods>               -> "{" <unspanned_seq-none-trait_method> "}" )
    ( <unspanned_seq-none-trait_method> -> )
    ( <unspanned_seq-none-trait_method> -> <trait_method> <unspanned_seq-none-trait_method> )
    ( <trait_method>                -> <outer_attributes> <staticness> <visibility> <fn_purity>
                                       <method_name> <ty_params> <fn_decl_with_self-arg_general-false> ";" )
    ( <trait_method>                -> <outer_attributes> <staticness> <visibility> <fn_purity>
                                       <method_name> <ty_params> <fn_decl_with_self-arg_general-false>
                                       <inner_attrs_and_block-true> )

    ( <fn_decl_with_self-arg_general-false> -> "(" "&" <explref_self_ty> "," <seq-comma-arg_general-false> ")" <ret_ty> )
    ( <fn_decl_with_self-arg_general-false> -> "(" "@" <explref_self_ty> "," <seq-comma-arg_general-false> ")" <ret_ty> )
    ( <fn_decl_with_self-arg_general-false> -> "(" "~" <explref_self_ty> "," <seq-comma-arg_general-false> ")" <ret_ty> )
    ( <fn_decl_with_self-arg_general-false> -> "(" "self"  ","               <seq-comma-arg_general-false> ")" <ret_ty> )
    ( <fn_decl_with_self-arg_general-false> -> "("                           <seq-comma-arg_general-false> ")" <ret_ty> )

    ( <explref_self_ty>             -> "const" "self" )
    ( <explref_self_ty>             -> "mut"   "self" )
    ( <explref_self_ty>             ->         "self" )

    ( <fn_block_decl>               -> "||" ...)
    ( <fn_block_decl>               -> "|" <seq-comma-fn_block_arg> "|" ... )

    ( <seq-comma-fn_block_arg>      -> )
    ( <seq-comma-fn_block_arg>      -> <neseq-comma-fn_block_arg> )
    ( <neseq-comma-fn_block_arg>    -> <fn_block_arg> "," <neseq-comma-fn_block_arg> )
    ( <neseq-comma-fn_block_arg>    -> <fn_block_arg> )

    ( <fn_block_arg>                -> <arg_mode> <maybe_mut> <pat> ":" <ty> )
    ( <fn_block_arg>                -> <arg_mode> <maybe_mut> <pat> )

    ( <maybe_mut>                   -> "mut" )
    ( <maybe_mut>                   -> )

    ( <block>                       -> <inner_attrs_and_block-false> )

    ( <inner_attrs_and_block-true>  -> <obsolete_unsafe> "{" <maybe_inner_attrs_and_next-true>  <block_tail> )
    ( <inner_attrs_and_block-false> -> <obsolete_unsafe> "{" <maybe_inner_attrs_and_next-false> <block_tail> )

    ( <maybe_inner_attrs_and_next-true> -> <inner_attrs_and_next> )
    ( <maybe_inner_attrs_and_next-false> -> )

    ( <block_tail>                  -> ...)

    ( <mod_items>                   -> ...)

    ( <doc_comment-style-outer>     -> ...)
    ( <doc_comment-style-inner>     -> ...)

    ( <obsolete_unsafe>             ->          )
    ( <obsolete_unsafe>             -> "unsafe" )
    ))
