use aleph_syntax_tree::syntax::AlephTree as at;
use std::collections::HashMap;

type Env = HashMap<String, String>;

fn in_env(x: &str, env: &Env) -> bool {
    find(x, env).is_some()
}

fn find(x: &str, env: &Env) -> Option<String> {
    env.get(x).cloned()
}

fn add_var(var: String, value: at, env: &Env) -> Env {
    let mut updated_env = env.clone();
    updated_env.insert(var, value.to_string_value());
    updated_env
}

fn is_simple(ast: at) -> bool {
    match ast {
        at::Int{..} | at::Float{..} | at::Bool{..} | at::String{..} | at::Ident{..} => true,
        _ => false,
    }
}

fn constant_folding_list(ast_list: Vec<Box<at>>, env: &mut Env) -> (Vec<Box<at>>, Env) {
    let mut folded_list = Vec::new();

    for ast in ast_list {
        let (folded_ast, modified_env) = constant_folding(*ast, env);
        folded_list.push(Box::new(folded_ast));
        *env = modified_env;  // Met à jour l'environnement modifié
    }

    (folded_list, env.clone())
}

fn constant_folding(ast: at, env: &Env) -> (at, Env) {
    match ast {
        at::Ident{value} => match in_env(&value, env) {
            true => (at::Int{value: find(&value, env).unwrap()}, env.clone()),
            false => (at::Ident{value: value}, env.clone()),
        },
        at::Let {var, is_pointer, value, expr} => {
            let (v1, e1) = constant_folding(*value, env);
            let v = match is_simple(v1.clone()) {
                true => match in_env(&v1.to_string_value(), &e1) {
                    true => at::Int{value: find(&v1.to_string_value(), &e1).unwrap()},
                    false => v1.clone(),
                },
                false => v1.clone(),
            };
            match v{
                at::Int{..} => (at::Let {var: var.clone(),is_pointer: is_pointer, value: Box::new(v), expr: expr}, add_var(var.clone(), v1.clone(), env)),
                at::Float{..} => (at::Let {var: var.clone(),is_pointer: is_pointer, value: Box::new(v), expr: expr}, add_var(var.clone(), v1.clone(), env)),
                at::Bool{..} => (at::Let {var: var.clone(),is_pointer: is_pointer, value: Box::new(v), expr: expr}, add_var(var.clone(), v1.clone(), env)),
                at::Ident{..} => (at::Let {var: var.clone(),is_pointer: is_pointer, value: Box::new(v), expr: expr}, add_var(var.clone(), v1.clone(), env)),
                _ => {
                    let (value_result, env2) = constant_folding(v1, &e1);
                    let updated_env = match is_simple(value_result.clone()) {
                        true => add_var(var.clone(), value_result.clone(), &env2),
                        false => env2.clone(),
                    };
                    let (expr_result, env3) = constant_folding(*expr, &updated_env);
                    (at::Let {var, is_pointer, value: Box::new(value_result), expr: Box::new(expr_result)}, env3)
                }
            }
        },
        at::Add {number_expr1, number_expr2} => {
            let (expr1_result, env2) = constant_folding(*number_expr1, env);
            let (expr2_result, env3) = constant_folding(*number_expr2, &env2);
            match(expr1_result.clone(), expr2_result.clone()) {
                (at::Int{value: v1}, at::Int{value: v2}) => (at::Int{value: (v1.parse::<i64>().unwrap()+v2.parse::<i64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Int{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()+v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Int{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()+v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()+v2.parse::<f64>().unwrap()).to_string()}, env3),
                _ => (at::Add{ number_expr1: Box::new(expr1_result), number_expr2: Box::new(expr2_result)}, env3),
            }
        },
        at::Sub {number_expr1, number_expr2} => {
            let (expr1_result, env2) = constant_folding(*number_expr1, env);
            let (expr2_result, env3) = constant_folding(*number_expr2, &env2);
            match(expr1_result.clone(), expr2_result.clone()) {
                (at::Int{value: v1}, at::Int{value: v2}) => (at::Int{value: (v1.parse::<i64>().unwrap()-v2.parse::<i64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Int{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()-v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Int{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()-v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()-v2.parse::<f64>().unwrap()).to_string()}, env3),
                _ => (at::Sub{ number_expr1: Box::new(expr1_result), number_expr2: Box::new(expr2_result)}, env3),
            }
        },
        at::Mul {number_expr1, number_expr2} => {
            let (expr1_result, env2) = constant_folding(*number_expr1, env);
            let (expr2_result, env3) = constant_folding(*number_expr2, &env2);
            match(expr1_result.clone(), expr2_result.clone()) {
                (at::Int{value: v1}, at::Int{value: v2}) => (at::Int{value: (v1.parse::<i64>().unwrap() * v2.parse::<i64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Int{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap() * v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Int{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap() * v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap() * v2.parse::<f64>().unwrap()).to_string()}, env3),
                _ => (at::Mul{ number_expr1: Box::new(expr1_result), number_expr2: Box::new(expr2_result)}, env3),
            }
        },
        at::Div{number_expr1, number_expr2} => {
            let (expr1_result, env2) = constant_folding(*number_expr1, env);
            let (expr2_result, env3) = constant_folding(*number_expr2, &env2);
            match(expr1_result.clone(), expr2_result.clone()) {
                (at::Int{value: v1}, at::Int{value: v2}) => (at::Int{value: (v1.parse::<i64>().unwrap()/v2.parse::<i64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Int{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()/v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Int{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()/v2.parse::<f64>().unwrap()).to_string()}, env3),
                (at::Float{value: v1}, at::Float{value: v2}) => (at::Float{value: (v1.parse::<f64>().unwrap()/v2.parse::<f64>().unwrap()).to_string()}, env3),
                _ => (at::Div{ number_expr1: Box::new(expr1_result), number_expr2: Box::new(expr2_result)}, env3),
            }
        },
        at::Neg{expr} => {
            let (expr_result, env2) = constant_folding(*expr, env);
            match expr_result.clone() {
                at::Int{value: v1} => (at::Int{value: (-(v1.parse::<i64>().unwrap())).to_string()}, env2),
                at::Float{value: v1} => (at::Float{value: ((-v1.parse::<f64>().unwrap())).to_string()}, env2),
                _ => (at::Neg{expr: Box::new(expr_result)}, env2),
            }
        },
        at::And{bool_expr1, bool_expr2} => {
            let (expr1_result, env2) = constant_folding(*bool_expr1, env);
            let (expr2_result, env3) = constant_folding(*bool_expr2, &env2);
            match(expr1_result.clone(), expr2_result.clone()) {
                (at::Bool{value: v1}, at::Bool{value: v2}) => (at::Bool{value: (v1.parse().unwrap() && v2.parse().unwrap()).to_string()}, env3),
                _ => (at::And{bool_expr1: Box::new(expr1_result), bool_expr2: Box::new(expr2_result)}, env3),
            }
        },
        at::Or{bool_expr1, bool_expr2} => {
            let (expr1_result, env2) = constant_folding(*bool_expr1, env);
            let (expr2_result, env3) = constant_folding(*bool_expr2, &env2);
            match(expr1_result.clone(), expr2_result.clone()) {
                (at::Bool{value: v1}, at::Bool{value: v2}) => (at::Bool{value: (v1.parse().unwrap() || v2.parse().unwrap()).to_string()}, env3),
                _ => (at::Or{bool_expr1: Box::new(expr1_result), bool_expr2: Box::new(expr2_result)}, env3),
            }
        },
        at::Not{bool_expr} => {
            let (expr_result, env2) = constant_folding(*bool_expr, env);
            match expr_result.clone() {
                at::Bool{value: v1} => (at::Bool{value: (!(v1.parse::<i64>().unwrap())).to_string()}, env2),
                _ => (at::Not{bool_expr: Box::new(expr_result)}, env2),
            }
        },
        at::Var {var, is_pointer} => {
            if in_env(&var, env) {
                let value = find(&var, env).unwrap();
                (at::Int {value: value.clone()}, env.clone())
            } else {
                (at::Var {var, is_pointer}, env.clone())
            }
        },
        at::Stmts {expr1, expr2} => {
            let (expr1_result, env2) = constant_folding(*expr1, env);
            let (expr2_result, env3) = constant_folding(*expr2, &env2);
            (at::Stmts { expr1: Box::new(expr1_result), expr2: Box::new(expr2_result)}, env3)
        },
        at::Eq{expr1, expr2} => {
            let (expr1_result, env2) = constant_folding(*expr1, env);
            let (expr2_result, env3) = constant_folding(*expr2, &env2);
            match(is_simple(expr1_result.clone()), is_simple(expr2_result.clone())) {
                (true, true) => match(expr1_result.clone(), expr2_result.clone()) {
                    (at::Ident{..}, _) => match expr1_result.to_string_value()==expr2_result.to_string_value() {
                        true => (at::Bool{value: "true".to_string()}, env3),
                        _ =>(at::Eq{expr1: Box::new(expr1_result), expr2: Box::new(expr2_result)}, env3),
                    },
                    (_, at::Ident{..}) => match expr1_result.to_string_value()==expr2_result.to_string_value() {
                        true => (at::Bool{value: "true".to_string()}, env3),
                        _ =>(at::Eq{expr1: Box::new(expr1_result), expr2: Box::new(expr2_result)}, env3),
                    },
                    _ =>(at::Bool{value: (expr1_result.to_string_value()==expr2_result.to_string_value()).to_string()}, env3),
                },
                _ => (at::Eq{expr1: Box::new(expr1_result), expr2: Box::new(expr2_result)}, env3)
            }
        },
        at::LE{expr1, expr2} => {
            let (expr1_result, env2) = constant_folding(*expr1, env);
            let (expr2_result, env3) = constant_folding(*expr2, &env2);
            match(is_simple(expr1_result.clone()), is_simple(expr2_result.clone())) {
                (true, true) => match(expr1_result.clone(), expr2_result.clone()) {
                    (at::Ident{..}, _) => match expr1_result.to_string_value()<=expr2_result.to_string_value() {
                        true => (at::Bool{value: "true".to_string()}, env3),
                        _ =>(at::LE{expr1: Box::new(expr1_result), expr2: Box::new(expr2_result)}, env3),
                    },
                    (_, at::Ident{..}) => match expr1_result.to_string_value()<=expr2_result.to_string_value() {
                        true => (at::Bool{value: "true".to_string()}, env3),
                        _ =>(at::LE{expr1: Box::new(expr1_result), expr2: Box::new(expr2_result)}, env3),
                    },
                    _ =>(at::Bool{value: (expr1_result.to_string_value()<=expr2_result.to_string_value()).to_string()}, env3),
                },
                _ => (at::LE{expr1: Box::new(expr1_result), expr2: Box::new(expr2_result)}, env3)
            }
        },
        at::If{condition, then, els} => {
            let (condition_result, env2) = constant_folding(*condition, env);
            let (then_result, env3) = constant_folding(*then, &env2);
            let (els_result, env4) = constant_folding(*els, &env3);
            match is_simple(condition_result.clone()) {
                true => match condition_result.clone().to_string_value().as_str() {
                    "true" => (then_result, env4),
                    "false" => (els_result, env4),
                    _ => (at::If{ condition: Box::new(condition_result), then: Box::new(then_result), els: Box::new(els_result)}, env4)
                },
                false => (at::If{ condition: Box::new(condition_result), then: Box::new(then_result), els: Box::new(els_result)}, env4)
            }
        },
        at::App{object_name, fun, param_list} => {
            let mut env_mut = env.clone();
            let(result_list, env_mut) = constant_folding_list(param_list, &mut env_mut);
            (at::App{object_name: object_name, fun: fun, param_list: result_list}, env_mut.clone())
        },
        _ => (ast, env.clone()),

    }
}
pub fn transform(ast: at) -> at {
    let (result, _) = constant_folding(ast, &HashMap::new());
    result
}


