use crate::ast::{GoatProgram, Identifier, ParameterType, IdentifierShapeDeclaration, Procedure};
use std::{collections::HashMap, collections::HashSet, error::Error};

pub fn semantic_analysis(program: GoatProgram) -> Result<(), Box<dyn Error>> {
    // Distinct names
    let mut names: HashSet<String> = HashSet::new();

    for procedure in &program.procedures {
        if names.contains(&procedure.identifier) {
            Err(format!("There is more than 1 proc with name {}", procedure.identifier))?
        }
        names.insert(procedure.identifier.clone());
    }

    let mut symbol_table: HashMap<Identifier, HashMap<Identifier, VariableInfo>> = HashMap::new();
    for procedure in &program.procedures {
        let mut vars = HashMap::new();

        // Formal parameters (for calls)
        for formal_param in &procedure.parameters {
            if vars.contains_key(&formal_param.identifier) {
                Err(format!("There is more than 1 formal parameter with name {}", procedure.identifier))?
            }

            let param = VariableInfo {
                r#type: formal_param.r#type,
                variable_location: VariableLocation::FormalParameter,
                shape: None
            };
            vars.insert(formal_param.identifier.clone(), param);
        }

        // Variable declarations (for each procedure)
        for variable_declaration in &procedure.variable_declarations {
            let identifier = match &variable_declaration.identifier_declaration {
                IdentifierShapeDeclaration::Identifier(identifier) => {
                    if vars.contains_key(identifier) {
                        Err(format!("There is more than 1 variable/parameter with name {}", identifier))?
                    }
                    identifier
                }
                IdentifierShapeDeclaration::IdentifierArray(identifier, _) => {
                    if vars.contains_key(identifier) {
                        Err(format!("There is more than 1 variable/parameter with name {}", identifier))?
                    }
                    identifier
                }
                IdentifierShapeDeclaration::IdentifierArray2D(identifier, _, _) => {
                    if vars.contains_key(identifier) {
                        Err(format!("There is more than 1 variable/parameter with name {}", identifier))?
                    }
                    identifier
                }
            };

            let param = VariableInfo {
                r#type: variable_declaration.r#type,
                variable_location: VariableLocation::VariableDeclaration,
                shape: Some(variable_declaration.identifier_declaration.clone())
            };
            vars.insert(identifier.clone(), param);
        }

        symbol_table.insert(procedure.identifier.clone(), vars);
    }

    for procedure in &program.procedures {
        analyse_procedure(&symbol_table, procedure)?;
    }

    Ok(())
}

fn analyse_procedure(symbol_table: &HashMap<Identifier, HashMap<Identifier, VariableInfo>>, procedure: &Procedure) -> Result<(), Box<dyn Error>> {

    Ok(())
}

struct VariableInfo {
    pub variable_location: VariableLocation,
    pub r#type: ParameterType,
    pub shape: Option<IdentifierShapeDeclaration>
}

enum VariableLocation {
    FormalParameter,
    VariableDeclaration
}
