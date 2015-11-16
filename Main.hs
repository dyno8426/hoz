import qualified Control.Exception as E
import SAS
import Environment
import SemStack

program_1 =	[
				LocalVar (Ident "x") 
				[
					BindVarToVal (Ident "x") (Value "1")
				], 
				LocalVar (Ident "y") 
				[
					BindVarToVal (Ident "y") (Value "2")
				]
			]
program_2 =	[
				Nop, 
				Nop, 
				Nop
			]
program_3 =	[
				LocalVar (Ident "x") 
				[
					LocalVar (Ident "y") 
					[
						LocalVar (Ident "z") 
						[
							Nop
						]
					]
				]
			]
program_4 =	[
				LocalVar (Ident "x") 
				[
					LocalVar (Ident "y") 
					[
						LocalVar (Ident "x") 
						[
							LocalVar (Ident "y") 
							[
								Nop
							]
						]
					]
				]
			]
program_5 =	[
				LocalVar (Ident "a") 
				[
					LocalVar (Ident "b") 
					[
						BindVarToVal (Ident "a") (Value "3"), 
						BindVarToVal (Ident "b") (Value "4")
					], 
					Nop
				], 
				Nop
			]
program_6 = [
				LocalVar (Ident "a") 
				[
					LocalVar (Ident "a") 
					[
						BindVarToVal (Ident "a") (Value "5"), 
						BindVarToVal (Ident "a") (Value "6")
					], 
					BindVarToVal (Ident "a") (Value "7")
				], 
				Nop
			]
program_7 =	[
				LocalVar (Ident "clause") 
				[
					LocalVar (Ident "p") 
					[
						BindVarToVal (Ident "clause") (Value "true"), 
						Conditional (Ident "clause") 
							[
								BindVarToVal (Ident "p") (Value "clause was true")
							] 
							[
								BindVarToVal (Ident "p") (Value "clause was false")
							]
					]
				]
			]
program_8 = [
				Nop, 
				LocalVar (Ident "a") 
				[
					BindVarToVal (Ident "a") (Value "1")
				], 
				LocalVar (Ident "b") 
				[
					BindVarToVal (Ident "b") (Value "2"), 
					BindVarToVal (Ident "a") (Value "3")
				], 
				Nop
			]
program_9 =	[
				LocalVar (Ident "result") 
				[
					LocalVar (Ident "variable") 
					[
						BindVarToVal (Ident "variable") (Value "40"), 
						OperateWithVal (Ident "result") (Ident "variable") Plus (Value "2")
					]
				]
			]
program_10 =	[
					LocalVar (Ident "d") 
					[
						LocalVar (Ident "e") 
						[
							LocalVar (Ident "f") 
							[
								BindVarToVal (Ident "e") (Value "5"), 
								BindVarToVal (Ident "f") (Value "6"), 
								OperateWithVar (Ident "d") (Ident "e") Plus (Ident "f")
							]
						]
					]
				]
program_11 = 	[
					LocalVar (Ident "p") 
					[
						LocalVar (Ident "q") 
						[
							BindVarToVar (Ident "p") (Ident "q"), 
							LocalVar (Ident "r") 
							[
								BindVarToVar (Ident "r") (Ident "p"), 
								BindVarToVal (Ident "q") (Value "42")
							]
						]
					]
				]
program_12 = 	[
					LocalVar (Ident "m") 
					[
						BindVarToVal (Ident "m") (Value "true"), 
						LocalVar (Ident "n") 
						[
							BindVarToVal (Ident "n") (Value "false"), 
							LocalVar (Ident "o") 
							[
								BindVarToVar (Ident "m") (Ident "n")
							]
						]
					]
				]
program_13 = 	[
					LocalVar (Ident "x") 
					[
						LocalVar (Ident "y") 
						[
							BindVarToVar (Ident "x") (Ident "y"), 
							BindVarToVal (Ident "x") (Value "true")
						]
					], 
					LocalVar (Ident "p") 
					[
						LocalVar (Ident "q") 
						[
							BindVarToVal (Ident "q") (Value "false"), 
							BindVarToVar (Ident "p") (Ident "q")
						]
					]
				]
program_14 = 	[
					LocalVar (Ident "x") 
					[
						BindVarToVal (Ident "x") (Value "alice"), 
						LocalVar (Ident "x") 
						[
							BindVarToVal (Ident "x") (Value "bob"), 
							LocalVar (Ident "x") 
							[
								BindVarToVal (Ident "x") (Value "alice and bob")
							]
						]
					]
				]
program_15 = 	[
					LocalVar (Ident "proc_1") 
					[
						LocalVar (Ident "x") 
						[
							BindVarToVal (Ident "x") (Value "42"), 
							BindVarToProc (Ident "proc_1") [(Ident "x"), (Ident "y")] 
							[
								OperateWithVal (Ident "y") (Ident "x") Plus (Value "1")
							]
						]
					]
				]
program_16 = 	[
					LocalVar (Ident "add_one") 
					[
						LocalVar (Ident "x") 
						[
							BindVarToVal (Ident "x") (Value "41"), 
							BindVarToProc (Ident "add_one") [(Ident "p_y")] 
							[
								OperateWithVal (Ident "p_y") (Ident "x") Plus (Value "1")
							], 
							LocalVar (Ident "x") 
							[
								LocalVar (Ident "y") 
								[
									BindVarToVal (Ident "x") (Value "99"), 
									Apply (Ident "add_one") [(Ident "y")]
								]
							]
						]
					]
				]
program_17 = 	[
					LocalVar (Ident "add_x_y") 
					[
						LocalVar (Ident "x") 
						[
							BindVarToProc (Ident "add_x_y") [(Ident "y"), (Ident "z")] 
							[
								OperateWithVar (Ident "z") (Ident "x") Plus (Ident "y")
							], 
							BindVarToVal (Ident "x") (Value "2")
						], 
						LocalVar (Ident "x") 
						[
							LocalVar (Ident "y") 
							[
								LocalVar (Ident "z") 
								[
									BindVarToVal (Ident "x") (Value "10"), 
									BindVarToVal (Ident "y") (Value "40"), 
									Apply (Ident "add_x_y") [(Ident "y"), (Ident "z")]
								]
							]
						]
					]
				]
program_18 = 	[
					LocalVar (Ident "add_x_y") 
					[
						LocalVar (Ident "x") 
						[
							BindVarToProc (Ident "add_x_y") [(Ident "y"), (Ident "z")] 
							[
								OperateWithVar (Ident "z") (Ident "x") Plus (Ident "y")
							], 
							BindVarToVal (Ident "x") (Value "2")
						], 
						LocalVar (Ident "x") 
						[
							LocalVar (Ident "y") 
							[
								LocalVar (Ident "z") 
								[
									BindVarToVal (Ident "x") (Value "10"), 
									BindVarToVal (Ident "y") (Value "40"), 
									Apply (Ident "x") [(Ident "y"), (Ident "z")]
								]
							]
						]
					]
				]
program_19 = 	[
					LocalVar (Ident "someone") 
					[
						LocalVar (Ident "name") 
						[
							LocalVar (Ident "id") 
							[
								BindVarToVal (Ident "id") (Value "786"), 
								BindVarToRec (Ident "someone") "employee" [("name",(Ident "name")), ("id",(Ident "id"))]
							], 
							BindVarToVal (Ident "name") (Value "ramu")
						]
					]
				]
program_20 =	[
					LocalVar (Ident "x") 
					[
						LocalVar (Ident "y") 
						[
							LocalVar (Ident "something") 
							[
								BindVarToRec (Ident "something") "person" [("gender",(Ident "x")), ("age",(Ident "y"))], 
								Case (Ident "something") "person" [("gender",(Ident "u"))] 
									[
										BindVarToVal (Ident "u") (Value "pattern was matched..."), 
										BindVarToVal (Ident "v") (Value "woohooo...")
									] 
									[
										BindVarToVal (Ident "x") (Value "pattern was not matched..."),
										BindVarToVal (Ident "y") (Value "project completed!")
									]
							]
						]
					]
				]
program_21 = 	[
					LocalVar (Ident "x") 
					[
						LocalVar (Ident "y") 
						[
							LocalVar (Ident "z") 
							[
								BindVarToVal (Ident "x") (Value "apples"), 
								BindVarToVal (Ident "y") (Value "oranges"), 
								BindVarToVal (Ident "z") (Value "berries"), 
								LocalVar (Ident "temp_1") 
								[
									LocalVar (Ident "temp_2") 
									[
										OperateWithVar (Ident "temp_1") (Ident "x") EqualEqualTo (Ident "y"), 
										OperateWithVar (Ident "temp_2") (Ident "y") NotEqualTo (Ident "z"), 
										LocalVar (Ident "result_1") 
										[
											Conditional (Ident "temp_1") 
											[
												BindVarToVal (Ident "result_1") (Value "apples are equal to oranges")
											] 
											[
												BindVarToVal (Ident "result_1") (Value "apples are not equal to oranges")
											]
										], 
										LocalVar (Ident "result_2") 
										[
											Conditional (Ident "temp_2") 
											[
												BindVarToVal (Ident "result_2") (Value "oranges are not equal to berries")
											] 
											[
												BindVarToVal (Ident "result_2") (Value "oranges are equal to berries")
											]
										]
									]
								]
							]
						]
					]
				]
program_22 =	[
					LocalVar (Ident "factorial") 
					[
						BindVarToProc (Ident "factorial") [(Ident "n"),(Ident "result")]	
						[
							LocalVar (Ident "temp") 
							[
								OperateWithVal (Ident "temp") (Ident "n") EqualEqualTo (Value "0"),
								Conditional (Ident "temp") [BindVarToVal (Ident "result") (Value "1")]
								[
									LocalVar (Ident "rec_result") 
									[
										LocalVar (Ident "rec_n") 
										[
											OperateWithVal (Ident "rec_n") (Ident "n") Minus (Value "1"),
											Apply (Ident "factorial") [(Ident "rec_n"), (Ident "rec_result")],
											OperateWithVar (Ident "result") (Ident "rec_result") Multiply (Ident "n")
										]
									]
								]
							]
						],
						LocalVar (Ident "n") 
						[
							BindVarToVal (Ident "n") (Value "5"),
							LocalVar (Ident "result") 
							[
								Apply (Ident "factorial") [(Ident "n"), (Ident "result")]
							]
						]
					]
				]
program_23 = 	[
					LocalVar (Ident "func")
					[
						LocalVar (Ident "arg_func")
						[
							BindVarToProc (Ident "arg_func") [(Ident "x"),(Ident "y")] 
							[
								OperateWithVal (Ident "y") (Ident "x") Multiply (Value "42")
							],
							BindVarToProc (Ident "func") [(Ident "x"),(Ident "arg_func"),(Ident "result")] 
							[
								Apply (Ident "arg_func") [(Ident "x"),(Ident "result")]
							],
							LocalVar (Ident "x") 
							[
								BindVarToVal (Ident "x") (Value "100"),
								LocalVar (Ident "result") 
								[
									Apply (Ident "func") [(Ident "x"),(Ident "arg_func"),(Ident "result")]
								]
							]
						]
					]
				]
program_24 =	[
					LocalVar (Ident "a") 
					[
						LocalVar (Ident "b") 
						[
							BindVarToVar (Ident "a") (Ident "b"), 
							BindVarToVal (Ident "b") (Value "42")
						]
					]
				]
(sas,eq_sets) = SAS.initializeSAS
env = Environment.initializeEnv
--sem_stack = pushToSemanticStack [] (reverse program_22) env

showProgram :: Int -> String
showProgram 1 = show program_1
showProgram 2 = show program_2
showProgram 3 = show program_3
showProgram 4 = show program_4
showProgram 5 = show program_5
showProgram 6 = show program_6
showProgram 7 = show program_7
showProgram 8 = show program_8
showProgram 9 = show program_9
showProgram 10 = show program_10
showProgram 11 = show program_11
showProgram 12 = show program_12
showProgram 13 = show program_13
showProgram 14 = show program_14
showProgram 15 = show program_15
showProgram 16 = show program_16
showProgram 17 = show program_17
showProgram 18 = show program_18
showProgram 19 = show program_19
showProgram 20 = show program_20
showProgram 21 = show program_21
showProgram 22 = show program_22
showProgram 23 = show program_23
showProgram 24 = show program_24
showProgram input = error $ concat ["Invalid input: ", "'program_", (show input), "' does not exist."]

selectProgram :: Int -> SemanticStack
selectProgram 1 = pushToSemanticStack [] (reverse program_1) env
selectProgram 2 = pushToSemanticStack [] (reverse program_2) env
selectProgram 3 = pushToSemanticStack [] (reverse program_3) env
selectProgram 4 = pushToSemanticStack [] (reverse program_4) env
selectProgram 5 = pushToSemanticStack [] (reverse program_5) env
selectProgram 6 = pushToSemanticStack [] (reverse program_6) env
selectProgram 7 = pushToSemanticStack [] (reverse program_7) env
selectProgram 8 = pushToSemanticStack [] (reverse program_8) env
selectProgram 9 = pushToSemanticStack [] (reverse program_9) env
selectProgram 10 = pushToSemanticStack [] (reverse program_10) env
selectProgram 11 = pushToSemanticStack [] (reverse program_11) env
selectProgram 12 = pushToSemanticStack [] (reverse program_12) env
selectProgram 13 = pushToSemanticStack [] (reverse program_13) env
selectProgram 14 = pushToSemanticStack [] (reverse program_14) env
selectProgram 15 = pushToSemanticStack [] (reverse program_15) env
selectProgram 16 = pushToSemanticStack [] (reverse program_16) env
selectProgram 17 = pushToSemanticStack [] (reverse program_17) env
selectProgram 18 = pushToSemanticStack [] (reverse program_18) env
selectProgram 19 = pushToSemanticStack [] (reverse program_19) env
selectProgram 20 = pushToSemanticStack [] (reverse program_20) env
selectProgram 21 = pushToSemanticStack [] (reverse program_21) env
selectProgram 22 = pushToSemanticStack [] (reverse program_22) env
selectProgram 23 = pushToSemanticStack [] (reverse program_23) env
selectProgram 24 = pushToSemanticStack [] (reverse program_24) env
selectProgram input = error $ concat ["Invalid input: ", "Program #", (show input), " does not exist."]

main = do
	putStrLn "Waiting for program code..."
	input <- getLine
	putStrLn "\nExecuting..."
	putStrLn (showProgram $ read input)
	putStrLn ""
	putStrLn $ executeProgram (selectProgram $ read input) sas eq_sets
	putStrLn ""
	putStrLn "Exiting..."
