// Ported from https://github.com/jamesmacaulay/elm-secd/blob/499a9f67ad5ee75c4255e9cab2a50016b03451af/SECD.elm
// ...except for the capability of having built-in "basic functions"

#[macro_use(conslist)]
extern crate im;

use std::sync::Arc;
use im::conslist::ConsList;


type Stack = ConsList<Value>;
type Environment = ConsList<( Arc<String>, Arc<Value> )>;
type Control = ConsList<Instruction>;

#[derive(Debug)]
enum Value {
    Atom(Arc<String>),
    Closure((Environment, Arc<BoundVariables>), Arc<Expression>),
    ListValue(ConsList<Value>),
}

#[derive(Debug)]
enum BoundVariables {
    SingleVariable(Arc<String>),
    VariableList(ConsList<String>),
}

#[derive(Debug)]
enum Expression {
    Identifier(Arc<String>),
    Lambda(Arc<BoundVariables>, Arc<Expression>),
    Combination(Arc<Expression>, Arc<Expression>),
}

#[derive(Debug)]
enum Instruction {
    Ap,
    AE(Arc<Expression>),
}

fn location(environment: &Environment, name: &str) -> Option<Arc<Value>> {
    environment.iter().find(|pair| *pair.0 == name).map(|pair| pair.1.clone())
}

fn assoc(bound_vars: &BoundVariables, value: &Arc<Value>) -> Result<Environment, String> {
    match bound_vars {
        &BoundVariables::VariableList(ref vars) => {
            match **value {
                Value::ListValue(ref values) => {
                    let pairs = vars.iter().zip(values.iter());
                    Ok(pairs.fold(conslist![], |acc, pair| {
                        acc.cons(pair)
                    }))
                },
                _ => {
                    Err("could not decompose non-list value into variable list".to_string())
                },
            }
        },
        &BoundVariables::SingleVariable(ref var) => {
            Ok(conslist![(var.clone(), value.clone())])
        },
    }
}

fn derive(new_env: &Environment, base_env: &Environment) -> Environment {
    new_env.append(base_env)
}

#[derive(Debug)]
struct SECD {
    stack : Stack,
    environment : Environment,
    control : Control,
    dump : Option<Arc<SECD>>,
}

impl SECD {
    fn new() -> SECD {
        SECD {
            stack: conslist![],
            environment: conslist![],
            control: conslist![],
            dump: None,
        }
    }

    fn push_instruction(&self, instruction: Instruction) -> Self {
        SECD {
            stack : self.stack.clone(),
            environment : self.environment.clone(),
            control : self.control.cons(instruction),
            dump : self.dump.clone(),
        }
    }

    fn is_done(&self) -> bool {
        self.control.is_empty() && self.dump.is_none()
    }

    fn transform(&self) -> Result<Self, String> {
        if let (Some(ref hs), true, &Some(ref dump)) = (self.stack.head(), self.control.is_empty(), &self.dump) {
            Ok(SECD {
                stack: dump.stack.cons(hs.clone()),
                environment: dump.environment.clone(),
                control: dump.control.clone(),
                dump: dump.dump.clone(),
            })
        } else if self.stack.is_empty() && self.control.is_empty() {
            Err("invalid state: empty control with empty stack".to_string())
        } else if self.control.is_empty() && self.dump.is_none() {
            Err("already finished!".to_string())
        } else if let Some((ref hc, ref tc)) = self.control.uncons() {
            match hc.as_ref() {
                &Instruction::AE(ref expression) => {
                    match expression.as_ref() {
                        &Expression::Identifier(ref name) => {
                            let value = location(&self.environment, name).unwrap_or(Arc::new(Value::Atom(name.clone())));
                            Ok(SECD {
                                stack: self.stack.cons(value),
                                environment: self.environment.clone(),
                                control: tc.clone(),
                                dump: self.dump.clone(),
                            })
                        },
                        &Expression::Lambda(ref bound_vars, ref body) => {
                            let closure = Value::Closure((self.environment.clone(), bound_vars.clone()), body.clone());
                            Ok(SECD {
                                stack: self.stack.cons(closure),
                                environment: self.environment.clone(),
                                control: tc.clone(),
                                dump: self.dump.clone(),
                            })
                        },
                        &Expression::Combination(ref rand, ref rator) => {
                            let control = conslist![
                                Instruction::AE(rator.clone()),
                                Instruction::AE(rand.clone()),
                                Instruction::Ap
                            ].append(tc.clone());
                            Ok(SECD {
                                stack: self.stack.clone(),
                                environment: self.environment.clone(),
                                control: control,
                                dump: self.dump.clone(),
                            })
                        }
                    }
                },
                &Instruction::Ap => {
                    if let Some((ref fst, ref snd, ref tts)) = self.stack.uncons2() {
                        if let &Value::Closure((ref closed_over_env, ref bound_vars), ref expression) = fst.as_ref() {
                            assoc(bound_vars, snd).and_then(|bound_vars_env| {
                                let new_env = derive(&bound_vars_env, closed_over_env);
                                let dump2 = Some(Arc::new(SECD {
                                    stack: tts.clone(),
                                    environment: self.environment.clone(),
                                    control: tc.clone(),
                                    dump: self.dump.clone(),
                                }));
                                Ok(SECD {
                                    stack: conslist![],
                                    environment: new_env,
                                    control: conslist![Instruction::AE(expression.clone())],
                                    dump: dump2,
                                })
                            })
                        } else {
                            Err("invalid state: Ap instruction without Closure and argument at top of stack".to_string())
                        }
                    } else {
                        Err("invalid state: Ap instruction without Closure and argument at top of stack".to_string())
                    }
                }
            }
        } else {
            Err("invalid state: ¯\\_(ツ)_/¯".to_string())
        }
    }

    fn run_until_done(self) -> Result<Self, String> {
        if self.is_done() {
            Ok(self)
        } else {
            self.transform().and_then(|state| state.run_until_done())
        }
    }
}


fn lambda(var_name: &str, expression: Arc<Expression>) -> Arc<Expression> {
    Arc::new(Expression::Lambda(
        Arc::new(BoundVariables::SingleVariable(Arc::new(var_name.to_string()))),
        expression
    ))
}

fn ident(name: &str) -> Arc<Expression> {
    Arc::new(Expression::Identifier(Arc::new(name.to_string())))
}

fn apply(rand: Arc<Expression>, rator: Arc<Expression>) -> Arc<Expression> {
    Arc::new(Expression::Combination(rand, rator))
}

fn hello_world_identity() -> SECD {
    SECD::new().push_instruction(
        Instruction::AE(
            apply(lambda("x", ident("x")), ident("hello_world"))
        )
    )
}

fn main() {
    println!("{:#?}", hello_world_identity().run_until_done());
}
