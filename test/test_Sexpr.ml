open QuickCheck

open TestUtil

(* PShow instances *)
module PShow_string = struct
	type t = string
	let show : t -> pretty_str =
		fun s fmt () ->
				Format.fprintf fmt "%s" s
end
module PShow_Sexpr = struct
	type t = Sexpr.t
	let show : t -> pretty_str =
		fun s fmt () ->
				Sexpr.write fmt s
end

(* Arbitrary instances *)
module Arbitrary_ascii_char = struct
	type t = string
	let arbitrary =
		choose_int (0x20, 0x7e) >>= fun c ->
				ret_gen (Char.chr c)
end
module Arbitrary_string = struct
	type t = string
	let arbitrary =
		choose_int (0x20, 0x7e) >>= fun a ->
				let str = String.create 1 in
				str.[0] <- Char.chr a;
				ret_gen str
end
module Arbitrary_symbol = struct
	type t = string
	let arbitrary =
		elements (TestUtil.setDiff (iota 0x20 0x7e) (List.append (iota 0x30 0x39) (List.map Char.code [' ';'(';')']))) >>= fun a ->
				let str = String.create 1 in
				str.[0] <- Char.chr a;
				ret_gen str
end

type t =
	| Sstring of string
	| Sident of string
	| Sint of int
	| Sfloat of float
	| Schar of char
	| Sexpr of t list

module Arbitrary_Sexpr = struct
	type t = Sexpr.t
	let arbitrary =
		let rec arb_sub depth =
			if depth <= 0
			then
				Arbitrary_string.arbitrary >>= fun str ->
						Arbitrary_symbol.arbitrary >>= fun sym ->
								Arbitrary_int.arbitrary >>= fun i ->
										Arbitrary_ascii_char.arbitrary >>= fun c ->
												Arbitrary_float.arbitrary >>= fun f ->
														oneof[
															ret_gen (Sexpr.Sstring str);
															ret_gen (Sexpr.Sident sym);
															ret_gen (Sexpr.Sint i);
															ret_gen (Sexpr.Schar c);
															ret_gen (Sexpr.Sfloat f);
															]
			else
				let arb_list = sized choose_int0 >>= vector (arb_sub (depth - 1))
				in
				Arbitrary_string.arbitrary >>= fun str ->
						Arbitrary_symbol.arbitrary >>= fun sym ->
								Arbitrary_int.arbitrary >>= fun i ->
										Arbitrary_ascii_char.arbitrary >>= fun c ->
												Arbitrary_float.arbitrary >>= fun f ->
														arb_list >>= fun e ->
																oneof[
																	oneof[
																		ret_gen (Sexpr.Sstring str);
																		ret_gen (Sexpr.Sident sym);
																		ret_gen (Sexpr.Sint i);
																		ret_gen (Sexpr.Schar c);
																		ret_gen (Sexpr.Sfloat f)
																		];
																	ret_gen (Sexpr.Sexpr e);
																	]
		in
		let arb_Sexpr =
			choose_int(0, 1) >>= fun i ->
					arb_sub i
		in
		let arb_list = sized choose_int0 >>= vector arb_Sexpr
		in
		arb_list >>= fun e ->
				ret_gen (Sexpr.Sexpr e)
end

let () = Random.init 10

(* Testables instances *)
module Testable_Sexpr_to_bool =
	Testable_fun
	(Arbitrary_Sexpr)
	(PShow_Sexpr)
	(Testable_bool) ;;
module Check_fun_Sexpr_to_bool = Check(Testable_Sexpr_to_bool)

module Arbitrary_Sexpr_list = Arbitrary_list(Arbitrary_Sexpr) ;;
module PShow_Sexpr_list = PShow_list(PShow_Sexpr) ;;
module Testable_fun_Sexpr_list_to_bool =
	Testable_fun
	(Arbitrary_Sexpr_list)
	(PShow_Sexpr_list)
	(Testable_bool) ;;
module Check_fun_Sexpr_list_to_bool = Check(Testable_fun_Sexpr_list_to_bool)

let gen_prop_equality to_string eq x y =
	if (eq x y)
	then
		true
	else
		(Format.eprintf "Mismatching: @,@[%s@ != %s@]\n@?"
				(to_string x)
				(to_string y);
			false)

let prop_read_write : 'a -> bool =
	fun x ->
			try
				let y =
					Sexpr.read
						(Stream.of_string
								(call_with_output_string
										(fun fmtr ->
													(Sexpr.write fmtr x))))
				in
				gen_prop_equality Std.dump Sexpr.eq_Sexpr x y
			with
			| Stream.Error s ->
					(Format.printf "Exception: Stream.Error %S@\n@?" s;
						false)

let () = Check_fun_Sexpr_to_bool.quickCheck prop_read_write

