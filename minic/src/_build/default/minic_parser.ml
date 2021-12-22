
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WHILE
    | VOID
    | SUB
    | SET
    | SEMI
    | RPAR
    | RETURN
    | RBRK
    | PUTCHAR
    | PTRI
    | PTRB
    | PLUS
    | ORL
    | OR
    | NOT
    | NE
    | MUL
    | LT
    | LPAR
    | LEN
    | LE
    | LBRK
    | INT
    | INCR
    | IF
    | IDENT of (
# 13 "minic_parser.mly"
       (string)
# 37 "minic_parser.ml"
  )
    | GT
    | GE
    | FOR
    | EQ
    | EOF
    | END
    | ELSE
    | DIV
    | DECR
    | CST of (
# 11 "minic_parser.mly"
       (int)
# 51 "minic_parser.ml"
  )
    | COMA
    | BOOL_CST of (
# 12 "minic_parser.mly"
       (bool)
# 57 "minic_parser.ml"
  )
    | BOOL
    | BEGIN
    | ANDL
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState147
  | MenhirState145
  | MenhirState139
  | MenhirState129
  | MenhirState127
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState117
  | MenhirState114
  | MenhirState112
  | MenhirState110
  | MenhirState107
  | MenhirState103
  | MenhirState100
  | MenhirState99
  | MenhirState96
  | MenhirState94
  | MenhirState88
  | MenhirState87
  | MenhirState80
  | MenhirState78
  | MenhirState75
  | MenhirState60
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState18
  | MenhirState12
  | MenhirState11
  | MenhirState10
  | MenhirState9
  | MenhirState0

# 1 "minic_parser.mly"
  

  open Lexing
  open Minic_ast


# 134 "minic_parser.ml"

let rec _menhir_goto_bloc_instr_form : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (instr : (Minic_ast.instr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.instr) = 
# 134 "minic_parser.mly"
                            ( instr )
# 152 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FOR ->
        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | IDENT _v ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | PUTCHAR ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | END ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (init : (Minic_ast.instr)), _startpos_init_), _, (test : (Minic_ast.expr))), _, (iter : (Minic_ast.instr)), _startpos_iter_), _, (s : (Minic_ast.seq))) = _menhir_stack in
        let _9 = () in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 154 "minic_parser.mly"
                                                                                                    ( For(init, test, iter, s) )
# 205 "minic_parser.ml"
         in
        _menhir_goto_bloc_instr_form _menhir_env _menhir_stack _menhir_s _v
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 215 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FOR ->
                        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | IDENT _v ->
                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IF ->
                        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | PUTCHAR ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | RETURN ->
                        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | WHILE ->
                        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | END ->
                        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (s1 : (Minic_ast.seq))), _, (s2 : (Minic_ast.seq))) = _menhir_stack in
        let _9 = () in
        let _8 = () in
        let _7 = () in
        let _5 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 151 "minic_parser.mly"
                                                                                            ( If(e, s1, s2) )
# 288 "minic_parser.ml"
         in
        _menhir_goto_bloc_instr_form _menhir_env _menhir_stack _menhir_s _v
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (s : (Minic_ast.seq))) = _menhir_stack in
        let _5 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 152 "minic_parser.mly"
                                                         ( While(e, s) )
# 302 "minic_parser.ml"
         in
        _menhir_goto_bloc_instr_form _menhir_env _menhir_stack _menhir_s _v
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (t : (Minic_ast.typ)), _startpos_t_), (f : (
# 13 "minic_parser.mly"
       (string)
# 317 "minic_parser.ml"
            )), _startpos_f_), _, (opt : ((string * Minic_ast.typ) list))), _, (vars : (Minic_ast.var_decl list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 128 "minic_parser.mly"
    ( { name=f; params=opt; return=t; locals=vars; code=s } )
# 326 "minic_parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PTRB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PTRI ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EOF ->
                _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr_form : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState96 | MenhirState107 | MenhirState122 | MenhirState114 | MenhirState117 | MenhirState112 | MenhirState103 | MenhirState100 | MenhirState78 | MenhirState9 | MenhirState10 | MenhirState11 | MenhirState12 | MenhirState60 | MenhirState18 | MenhirState21 | MenhirState54 | MenhirState48 | MenhirState52 | MenhirState50 | MenhirState23 | MenhirState46 | MenhirState44 | MenhirState34 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState32 | MenhirState25 | MenhirState30 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Minic_ast.expr)) = _v in
        let _startpos_e_ = _startpos in
        let _v : (Minic_ast.expr) = 
# 179 "minic_parser.mly"
              ( e )
# 369 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState94 | MenhirState99 | MenhirState110 | MenhirState139 | MenhirState120 | MenhirState124 | MenhirState127 | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Minic_ast.expr)) = _v in
        let _startpos_e_ = _startpos in
        let _startpos = _startpos_e_ in
        let _v : (Minic_ast.instr) = 
# 147 "minic_parser.mly"
              ( Expr(e) )
# 381 "minic_parser.ml"
         in
        _menhir_goto_instr_form _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_instr_form : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | BOOL_CST _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | CST _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | IDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | LPAR ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | SUB ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FOR ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | IDENT _v ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IF ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | PUTCHAR ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHILE ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | END ->
                    _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 | MenhirState99 | MenhirState110 | MenhirState139 | MenhirState127 | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (instr : (Minic_ast.instr)), _startpos_instr_) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.instr) = 
# 133 "minic_parser.mly"
                        ( instr )
# 486 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | END | FOR | IDENT _ | IF | PUTCHAR | RETURN | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Minic_ast.instr)), _startpos__1_) = _menhir_stack in
            let _v : (Minic_ast.instr) = let _startpos = _startpos__1_ in
            
# 135 "minic_parser.mly"
             ( let pos = _startpos in
               let message =
                  Printf.sprintf
                  "Missing \";\" after instruction at ( %d : %d )"
                  pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
               in
               failwith message )
# 502 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 519 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | BOOL_CST _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | CST _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BOOL_CST _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | CST _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | BOOL_CST _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | CST _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "minic_parser.mly"
       (string)
# 667 "minic_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DECR ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
    | INCR ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
    | LBRK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | BOOL_CST _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | CST _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | LPAR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | BOOL_CST _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | CST _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PUTCHAR ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_list_variable_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.var_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.var_decl))), _, (xs : (Minic_ast.var_decl list))) = _menhir_stack in
        let _v : (Minic_ast.var_decl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1166 "minic_parser.ml"
         in
        _menhir_goto_list_variable_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | IDENT _v ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PUTCHAR ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | END ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | _ ->
        _menhir_fail ()

and _menhir_goto_args_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (Minic_ast.expr))), _, (dl : (Minic_ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr list) = 
# 195 "minic_parser.mly"
                                  ( vd :: dl )
# 1207 "minic_parser.ml"
         in
        _menhir_goto_args_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (l : (Minic_ast.expr list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 161 "minic_parser.mly"
                        ( let arr = Array.of_list l in Array(arr) )
# 1225 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f : (
# 13 "minic_parser.mly"
       (string)
# 1246 "minic_parser.ml"
            )), _startpos_f_), _, (params : (Minic_ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_f_ in
            let _v : (Minic_ast.expr) = 
# 189 "minic_parser.mly"
                                     ( Call(f, params) )
# 1254 "minic_parser.ml"
             in
            _menhir_goto_expr_form _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (vector : (string * Minic_ast.typ)), _startpos_vector_), _, (s : (Minic_ast.expr list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _startpos = _startpos_vector_ in
            let _v : (Minic_ast.var_decl) = 
# 95 "minic_parser.mly"
                                          ( let init = Array.of_list s in (fst vector, snd vector, (Some (Array(init)))) )
# 1280 "minic_parser.ml"
             in
            _menhir_goto_variable_decl_form _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.var_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 | MenhirState75 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PTRB ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PTRI ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState88 | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PTRB ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PTRI ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | END | FOR | IDENT _ | IF | PUTCHAR | RETURN | WHILE ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 13 "minic_parser.mly"
       (string)
# 1344 "minic_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | RPAR ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 13 "minic_parser.mly"
       (string)
# 1376 "minic_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (id : (
# 13 "minic_parser.mly"
       (string)
# 1384 "minic_parser.ml"
    )), _startpos_id_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_id_ in
    let _v : (Minic_ast.expr) = 
# 187 "minic_parser.mly"
                ( Incr( id ) )
# 1391 "minic_parser.ml"
     in
    _menhir_goto_expr_form _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 13 "minic_parser.mly"
       (string)
# 1398 "minic_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (id : (
# 13 "minic_parser.mly"
       (string)
# 1406 "minic_parser.ml"
    )), _startpos_id_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_id_ in
    let _v : (Minic_ast.expr) = 
# 188 "minic_parser.mly"
                ( Decr( id ) )
# 1413 "minic_parser.ml"
     in
    _menhir_goto_expr_form _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 | MenhirState18 | MenhirState54 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | BOOL_CST _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | CST _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | IDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LPAR ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | SUB ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | END | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.expr list) = 
# 194 "minic_parser.mly"
               ( [e] )
# 1489 "minic_parser.ml"
             in
            _menhir_goto_args_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | BOOL | COMA | END | EOF | FOR | IDENT _ | IF | INT | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 178 "minic_parser.mly"
                                 ( Xor(a, b) )
# 1532 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | GE | GT | IDENT _ | IF | INT | LE | LT | NE | OR | ORL | PLUS | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | SUB | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 165 "minic_parser.mly"
                                 ( Sub(a, b) )
# 1557 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr) = 
# 162 "minic_parser.mly"
                                 ( Mul(a, b) )
# 1574 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr) = 
# 164 "minic_parser.mly"
                                 ( Div(a, b) )
# 1585 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | GE | GT | IDENT _ | IF | INT | LE | LT | NE | OR | ORL | PLUS | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | SUB | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 163 "minic_parser.mly"
                                 ( Add(a, b) )
# 1604 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | IDENT _ | IF | INT | NE | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 170 "minic_parser.mly"
                                 ( Ne(a, b) )
# 1641 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | GE | GT | IDENT _ | IF | INT | LE | LT | NE | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 167 "minic_parser.mly"
                                 ( Lt(a, b) )
# 1670 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | GE | GT | IDENT _ | IF | INT | LE | LT | NE | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 171 "minic_parser.mly"
                                 ( Le(a, b) )
# 1699 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | GE | GT | IDENT _ | IF | INT | LE | LT | NE | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 168 "minic_parser.mly"
                                 ( Gt(a, b) )
# 1728 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | GE | GT | IDENT _ | IF | INT | LE | LT | NE | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 172 "minic_parser.mly"
                                 ( Ge(a, b) )
# 1757 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | EQ | FOR | IDENT _ | IF | INT | NE | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 169 "minic_parser.mly"
                                 ( Eq(a, b) )
# 1794 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | AND | ANDL | BOOL | COMA | END | EOF | FOR | IDENT _ | IF | INT | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 176 "minic_parser.mly"
                                 ( And(a, b) )
# 1835 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | BOOL | COMA | END | EOF | FOR | IDENT _ | IF | INT | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 175 "minic_parser.mly"
                                 ( Orl(a, b) )
# 1884 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | BOOL | COMA | END | EOF | FOR | IDENT _ | IF | INT | OR | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 177 "minic_parser.mly"
                                 ( Or(a, b) )
# 1929 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | ANDL | BOOL | COMA | END | EOF | FOR | IDENT _ | IF | INT | ORL | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 174 "minic_parser.mly"
                                 ( Andl(a, b) )
# 1976 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RBRK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (tab : (
# 13 "minic_parser.mly"
       (string)
# 2023 "minic_parser.ml"
            )), _startpos_tab_), _, (n : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 182 "minic_parser.mly"
                                   ( Elm(n, tab) )
# 2030 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 181 "minic_parser.mly"
                         ( Par(e) )
# 2084 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (b : (Minic_ast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Minic_ast.expr) = 
# 173 "minic_parser.mly"
                    ( Not(b) )
# 2105 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (a : (Minic_ast.expr))) = _menhir_stack in
        let _1 = () in
        let _v : (Minic_ast.expr) = 
# 166 "minic_parser.mly"
                                ( Opp(a) )
# 2116 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | BOOL | END | EOF | FOR | IDENT _ | IF | INT | PTRB | PTRI | PUTCHAR | RETURN | SEMI | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (typ : (Minic_ast.typ)), _startpos_typ_), (var : (
# 13 "minic_parser.mly"
       (string)
# 2159 "minic_parser.ml"
            )), _startpos_var_), _, (init : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _startpos = _startpos_typ_ in
            let _v : (Minic_ast.var_decl) = 
# 94 "minic_parser.mly"
                                        ( (var, typ, Some(init)) )
# 2166 "minic_parser.ml"
             in
            _menhir_goto_variable_decl_form _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FOR ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | IDENT _v ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IF ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | PUTCHAR ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHILE ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | END ->
                    _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | END | FOR | IDENT _ | IF | PUTCHAR | RETURN | RPAR | SEMI | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (Minic_ast.instr) = 
# 148 "minic_parser.mly"
                      ( Return(e) )
# 2293 "minic_parser.ml"
             in
            _menhir_goto_instr_form _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (Minic_ast.instr) = 
# 145 "minic_parser.mly"
                                 ( Putchar(e) )
# 2345 "minic_parser.ml"
             in
            _menhir_goto_instr_form _menhir_env _menhir_stack _menhir_s _v _startpos
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FOR ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | IDENT _v ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IF ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | PUTCHAR ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHILE ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | END ->
                    _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | END | FOR | IDENT _ | IF | PUTCHAR | RETURN | RPAR | SEMI | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 13 "minic_parser.mly"
       (string)
# 2473 "minic_parser.ml"
            )), _startpos_id_), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_id_ in
            let _v : (Minic_ast.instr) = 
# 146 "minic_parser.mly"
                            ( Set(id, e) )
# 2480 "minic_parser.ml"
             in
            _menhir_goto_instr_form _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RBRK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | BOOL_CST _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | CST _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | IDENT _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | LPAR ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | NOT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | SUB ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | END | FOR | IDENT _ | IF | PUTCHAR | RETURN | RPAR | SEMI | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (tab : (
# 13 "minic_parser.mly"
       (string)
# 2606 "minic_parser.ml"
            )), _startpos_tab_), _, (pos : (Minic_ast.expr))), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_tab_ in
            let _v : (Minic_ast.instr) = 
# 144 "minic_parser.mly"
                                                      ( Insert(pos, e, tab) )
# 2615 "minic_parser.ml"
             in
            _menhir_goto_instr_form _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ANDL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ORL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PUTCHAR ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.var_decl list) = 
# 211 "<standard.mly>"
    ( [] )
# 2688 "minic_parser.ml"
     in
    _menhir_goto_list_variable_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.expr list) = 
# 193 "minic_parser.mly"
              ( [] )
# 2697 "minic_parser.ml"
     in
    _menhir_goto_args_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_variable_decl_form : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.var_decl) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (var_dcl : (Minic_ast.var_decl)), _startpos_var_dcl_) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.var_decl) = 
# 84 "minic_parser.mly"
                                  ( var_dcl )
# 2717 "minic_parser.ml"
         in
        _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
    | BOOL | END | EOF | FOR | IDENT _ | IF | INT | PTRB | PTRI | PUTCHAR | RETURN | VOID | WHILE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Minic_ast.var_decl)), _startpos__1_) = _menhir_stack in
        let _v : (Minic_ast.var_decl) = let _startpos = _startpos__1_ in
        
# 85 "minic_parser.mly"
                     ( let pos = _startpos in
                        let message =
                           Printf.sprintf
                           "Missing \";\" after instruction at ( %d : %d )"
                           pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
                        in
                        failwith message )
# 2733 "minic_parser.ml"
         in
        _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), (tab : (
# 13 "minic_parser.mly"
       (string)
# 2849 "minic_parser.ml"
                )), _startpos_tab_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.expr) = 
# 183 "minic_parser.mly"
                             ( Len(tab) )
# 2857 "minic_parser.ml"
                 in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "minic_parser.mly"
       (string)
# 2882 "minic_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DECR ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
    | INCR ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
    | LBRK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | BOOL_CST _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | CST _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | IDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NOT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SUB ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | LPAR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
    | AND | ANDL | BOOL | COMA | DIV | END | EOF | EQ | FOR | GE | GT | IDENT _ | IF | INT | LE | LT | MUL | NE | OR | ORL | PLUS | PTRB | PTRI | PUTCHAR | RBRK | RETURN | RPAR | SEMI | SUB | VOID | WHILE | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (n : (
# 13 "minic_parser.mly"
       (string)
# 2925 "minic_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _v : (Minic_ast.expr) = 
# 180 "minic_parser.mly"
            ( Get(n) )
# 2930 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "minic_parser.mly"
       (int)
# 2943 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 11 "minic_parser.mly"
       (int)
# 2951 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 159 "minic_parser.mly"
        ( Cst(n) )
# 2956 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "minic_parser.mly"
       (bool)
# 2963 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 12 "minic_parser.mly"
       (bool)
# 2971 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 160 "minic_parser.mly"
             ( BCst(b) )
# 2976 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | END ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_goto_param_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | PTRB ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | PTRI ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | END | FOR | IDENT _ | IF | PUTCHAR | RETURN | WHILE ->
                    _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (string * Minic_ast.typ))), _, (dl : ((string * Minic_ast.typ) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Minic_ast.typ) list) = 
# 106 "minic_parser.mly"
                                  ( vd :: dl )
# 3064 "minic_parser.ml"
         in
        _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_vector : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Minic_ast.typ) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (string * Minic_ast.typ)), _startpos_t_) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _startpos = _startpos_t_ in
            let _v : (string * Minic_ast.typ) = 
# 100 "minic_parser.mly"
                    ( (fst(t), Tab(snd t)) )
# 3093 "minic_parser.ml"
             in
            _menhir_goto_vector _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | BOOL_CST _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | CST _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | IDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LPAR ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | SUB ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | END ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_param_dcl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PTRB ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PTRI ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (p : (string * Minic_ast.typ))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ) list) = 
# 105 "minic_parser.mly"
              ( [p] )
# 3186 "minic_parser.ml"
         in
        _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce67 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ) * Lexing.position) * (
# 13 "minic_parser.mly"
       (string)
# 3199 "minic_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (typ : (Minic_ast.typ)), _startpos_typ_), (var : (
# 13 "minic_parser.mly"
       (string)
# 3205 "minic_parser.ml"
    )), _startpos_var_) = _menhir_stack in
    let _startpos = _startpos_typ_ in
    let _v : (Minic_ast.var_decl) = 
# 96 "minic_parser.mly"
                    ( (var, typ, None) )
# 3211 "minic_parser.ml"
     in
    _menhir_goto_variable_decl_form _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run78 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ) * Lexing.position) * (
# 13 "minic_parser.mly"
       (string)
# 3218 "minic_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | BOOL_CST _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CST _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SUB ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_reduce53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Minic_ast.typ) list) = 
# 104 "minic_parser.mly"
              ( [] )
# 3250 "minic_parser.ml"
     in
    _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run91 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ) * Lexing.position) * (
# 13 "minic_parser.mly"
       (string)
# 3257 "minic_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (typ : (Minic_ast.typ)), _startpos_typ_), (tab : (
# 13 "minic_parser.mly"
       (string)
# 3270 "minic_parser.ml"
        )), _startpos_tab_) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _startpos = _startpos_typ_ in
        let _v : (string * Minic_ast.typ) = 
# 99 "minic_parser.mly"
                             ( (tab, Tab(typ)) )
# 3278 "minic_parser.ml"
         in
        _menhir_goto_vector _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 3291 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 3299 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.var_decl list * Minic_ast.fun_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (fd : (Minic_ast.fun_def))), _, (dl : (Minic_ast.var_decl list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : (Minic_ast.var_decl list * Minic_ast.fun_def list) = 
# 77 "minic_parser.mly"
                                       ( let vl, fl = dl in
                                         vl, fd :: fl )
# 3315 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (Minic_ast.var_decl))), _, (dl : (Minic_ast.var_decl list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : (Minic_ast.var_decl list * Minic_ast.fun_def list) = 
# 75 "minic_parser.mly"
                                       ( let vl, fl = dl in
                                         vd :: vl, fl )
# 3326 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (dl : (Minic_ast.var_decl list * Minic_ast.fun_def list))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 3342 "minic_parser.ml"
            ) = 
# 61 "minic_parser.mly"
       ( let var_list, fun_list = dl in
         { globals = var_list; functions = fun_list; } )
# 3347 "minic_parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState0 | MenhirState147 | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRK ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | PTRB ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | PTRI ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RPAR ->
                    _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
            | SET ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
            | BOOL | EOF | INT | PTRB | PTRI | SEMI | VOID ->
                _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRK ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRK ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ)), _startpos_t_), (x : (
# 13 "minic_parser.mly"
       (string)
# 3438 "minic_parser.ml"
                    )), _startpos_x_) = _menhir_stack in
                    let _4 = () in
                    let _3 = () in
                    let _v : (string * Minic_ast.typ) = 
# 110 "minic_parser.mly"
                         ( (x, Tab(t)) )
# 3445 "minic_parser.ml"
                     in
                    _menhir_goto_param_dcl _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | COMA | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ)), _startpos_t_), (x : (
# 13 "minic_parser.mly"
       (string)
# 3459 "minic_parser.ml"
                )), _startpos_x_) = _menhir_stack in
                let _v : (string * Minic_ast.typ) = 
# 109 "minic_parser.mly"
               ( (x, t) )
# 3464 "minic_parser.ml"
                 in
                _menhir_goto_param_dcl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRK ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
            | BOOL | END | FOR | IDENT _ | IF | INT | PTRB | PTRI | PUTCHAR | RETURN | SEMI | VOID | WHILE ->
                _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _1 = () in
        let _v : (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 3710 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 63 "minic_parser.mly"
        ( let pos = _startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message )
# 3721 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.var_decl list * Minic_ast.fun_def list) = 
# 74 "minic_parser.mly"
             ( [], [] )
# 3730 "minic_parser.ml"
     in
    _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (Minic_ast.typ) = 
# 117 "minic_parser.mly"
       ( Void )
# 3744 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (Minic_ast.typ) = 
# 119 "minic_parser.mly"
       ( Ptr(Int) )
# 3758 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (Minic_ast.typ) = 
# 118 "minic_parser.mly"
       ( Ptr(Bool) )
# 3772 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (Minic_ast.typ) = 
# 115 "minic_parser.mly"
      ( Int )
# 3786 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (Minic_ast.typ) = 
# 116 "minic_parser.mly"
       ( Bool )
# 3800 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 41 "minic_parser.mly"
      (Minic_ast.prog)
# 3819 "minic_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PTRB ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PTRI ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3853 "minic_parser.ml"
