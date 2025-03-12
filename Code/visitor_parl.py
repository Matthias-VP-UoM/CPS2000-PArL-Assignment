import astnodes_parl as ast

class ASTVisitor:
    def visit_datatype_node(self, node):
        raise NotImplementedError()

    def visit_integer_node(self, node):
        raise NotImplementedError()
    
    def visit_bool_node(self, node):
        raise NotImplementedError()
    
    def visit_float_node(self, node):
        raise NotImplementedError()
    
    def visit_colour_node(self, node):
        raise NotImplementedError()
    
    def visit_width_node(self, node):
        raise NotImplementedError()
    
    def visit_height_node(self, node):
        raise NotImplementedError()
    
    def visit_array_node(self, node):
        raise NotImplementedError()
    
    def visit_end_node(self, node):
        raise NotImplementedError()

    def visit_assignment_node(self, node):
        raise NotImplementedError()
    
    def visit_assignment_array_node(self, node):
        raise NotImplementedError()
    
    def visit_var_decl_node(self, node):
        raise NotImplementedError()
    
    def visit_var_decl_array_node(self, node):
        raise NotImplementedError()
    
    def visit_condition_node(self, node):
        raise NotImplementedError()
    
    def visit_return_node(self, node):
        raise NotImplementedError()
    
    def visit_variable_node(self, node):
        raise NotImplementedError()
    
    def visit_mod_expr_node(self, node):
        raise NotImplementedError()
    
    def visit_array_element_node(self, node):
        raise NotImplementedError()
    
    def visit_block_node(self, node):
        raise NotImplementedError()
    
    def visit_program_node(self, node):
        raise NotImplementedError()
    
    def visit_operator_node(self, node):
        raise NotImplementedError()
    
    def visit_binary_operator_node(self, node):
        raise NotImplementedError()
    
    def visit_if_node(self, node):
        raise NotImplementedError()
    
    def visit_if_else_node(self, node):
        raise NotImplementedError()
    
    def visit_for_node(self, node):
        raise NotImplementedError()
    
    def visit_while_node(self, node):
        raise NotImplementedError()
    
    def visit_function_decl_node(self, node):
        raise NotImplementedError()
    
    def visit_function_decl_with_params_node(self, node):
        raise NotImplementedError()
    
    def visit_function_node(self, node):
        raise NotImplementedError()
    
    def visit_function_call_node(self, node):
        raise NotImplementedError()
    
    def visit_parenthesis_node(self, node):
        raise NotImplementedError()
    
    def visit_print_node(self, node):
        raise NotImplementedError()
    
    def visit_delay_node(self, node):
        raise NotImplementedError()
    
    def visit_clear_node(self, node):
        raise NotImplementedError()
    
    def visit_write_node(self, node):
        raise NotImplementedError()
    
    def visit_write_box_node(self, node):
        raise NotImplementedError()
    
    def visit_random_node(self, node):
        raise NotImplementedError()
    
    def inc_tab_count(self):
        raise NotImplementedError()
    
    def dec_tab_count(self):
        raise NotImplementedError()

class PrintNodesVisitor(ASTVisitor):
    def __init__(self):
        self.name = "Print Tree Visitor"
        self.node_count = 0
        self.tab_count = 0

    def inc_tab_count(self):
        self.tab_count += 1

    def dec_tab_count(self):
        self.tab_count -= 1
    
    def visit_datatype_node(self, type_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Data Type::", type_node.type)
        
    def visit_integer_node(self, int_node):
        self.node_count += 1
        if not isinstance(int_node, ast.ASTVariableNode):
            print('\t' * self.tab_count, "Integer Value::", int_node.value)
        else:
            self.visit_variable_node(int_node)
    
    def visit_bool_node(self, bool_node):
        if not isinstance(bool_node, ast.ASTVariableNode):
            print('\t' * self.tab_count, "Boolean Value::", bool_node.value)
        else:
            self.visit_variable_node(bool_node)

    def visit_float_node(self, float_node):
        self.node_count += 1
        if not isinstance(float_node, ast.ASTVariableNode):
            print('\t' * self.tab_count, "Float Value::", float_node.value)
        else:
            self.visit_variable_node(float_node)
    
    def visit_colour_node(self, colour_node):
        self.node_count += 1
        if not isinstance(colour_node, ast.ASTVariableNode):
            if isinstance(colour_node.value, str):
                colour_node.value = int(colour_node.value[1:], 16)
            print('\t' * self.tab_count, "Colour Value::", colour_node.value, "(#"+str(hex(colour_node.value))[2:]+")")
        else:
            self.visit_variable_node(colour_node)
    
    def visit_width_node(self, width_node):
        self.node_count += 1
        if not isinstance(width_node, ast.ASTVariableNode):
            print('\t' * self.tab_count, "Width Value::", width_node.value)
        else:
            self.visit_variable_node(width_node)
    
    def visit_height_node(self, height_node):
        self.node_count += 1
        if not isinstance(height_node, ast.ASTVariableNode):
            print('\t' * self.tab_count, "Height Value::", height_node.value)
        else:
            self.visit_variable_node(height_node)
    
    def visit_array_node(self, arr_node):
        self.node_count += 1
        for val in arr_node.values:
            val.accept(self)
        print('\t' * self.tab_count, "Array Length => ", arr_node.length.value)

    def visit_assignment_node(self, ass_node):
        self.node_count += 1
        print('\t' * self.tab_count, "ASSIGNMENT Node => ")
        self.inc_tab_count()        
        ass_node.id.accept(self)
        ass_node.expr.accept(self)
        self.dec_tab_count()
    
    def visit_assignment_array_node(self, decl_node):
        self.node_count += 1
        print('\t' * self.tab_count, "ASSIGNMENT ARRAY Node => ")
        self.inc_tab_count()        
        decl_node.id.accept(self)
        decl_node.expr.accept(self)
        self.dec_tab_count()
    
    def visit_mod_expr_node(self, expr_node):
        self.node_count += 1
        print('\t' * self.tab_count, "MODIFIED EXPRESSION Node => ")
        self.inc_tab_count()
        expr_node.id.accept(self)
        expr_node.operator.accept(self)
        expr_node.expr.accept(self)
    
    def visit_return_node(self, ret_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Return Node => ")
        self.inc_tab_count()        
        ret_node.val.accept(self)
        self.dec_tab_count()
    
    def visit_var_decl_node(self, decl_node):
        self.node_count += 1
        print('\t' * self.tab_count, "VARIABLE DECLARATION Node => ")
        self.inc_tab_count()        
        decl_node.id.accept(self)
        decl_node.type.accept(self)
        decl_node.expr.accept(self)
        self.dec_tab_count()
    
    def visit_var_decl_array_node(self, decl_node):
        self.node_count += 1
        print('\t' * self.tab_count, "VARIABLE DECLARATION ARRAY Node => ")
        self.inc_tab_count()        
        decl_node.id.accept(self)
        decl_node.type.accept(self)
        decl_node.expr.accept(self)
        self.dec_tab_count()
    
    def visit_operator_node(self, op_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Operator => ", op_node.operator)

    def visit_binary_operator_node(self, op_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Relational Operator => ", op_node.binOp)
    
    def visit_if_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "IF Statement => ")
        self.inc_tab_count() 
        #print("IF Statement:")
        if node.condition is not None:
            node.condition.accept(self)
        else:
            print('\t' * self.tab_count, "Condition: ", node.cond_full)
        print('\t' * self.tab_count, "Body:")
        self.visit_block_node(node.if_body)
        self.dec_tab_count()

    def visit_if_else_node(self, node):
        self.node_count += 1
        self.node_count += 1
        print('\t' * self.tab_count, "IF-ELSE Statement => ")
        self.inc_tab_count() 
        #print("IF-ELSE Statement:")
        if node.condition is not None:
            node.condition.accept(self)
        else:
            print('\t' * self.tab_count, "Condition: ", node.cond_full)
        print('\t' * self.tab_count, "IF Body:")
        self.visit_block_node(node.if_body)
        print('\t' * self.tab_count, "ELSE Body:")
        self.visit_block_node(node.else_body)
        self.dec_tab_count()
    
    def visit_for_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "FOR Statement => ")
        self.inc_tab_count() 
        node.var_init.accept(self)
        if node.condition is not None:
            node.condition.accept(self)
        else:
            print('\t' * self.tab_count, "Condition: ", node.cond_full)
        node.var_inc.accept(self)
        print('\t' * self.tab_count, "Body:")
        self.visit_block_node(node.for_body)
        self.dec_tab_count()
    
    def visit_while_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "WHILE Statement => ")
        self.inc_tab_count() 
        if node.condition is not None:
            node.condition.accept(self)
        else:
            print('\t' * self.tab_count, "Condition: ", node.cond_full)
        print('\t' * self.tab_count, "Body:")
        self.visit_block_node(node.while_body)
        self.dec_tab_count()

    def visit_variable_node(self, var_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Variable => ", var_node.lexeme)
    
    def visit_array_element_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "Array Element Value => ")
        self.inc_tab_count() 
        print('\t' * self.tab_count, "Array => ")
        self.inc_tab_count()
        node.arr_node.accept(self)
        self.dec_tab_count()
        print('\t' * self.tab_count, "Index => ", node.index.value)
        print('\t' * self.tab_count, "Value => ")
        self.inc_tab_count()
        node.arr_node.values[int(node.index.value)].accept(self)
        self.dec_tab_count()
        self.dec_tab_count()
    
    def visit_function_decl_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "Function Declaration => ")
        self.inc_tab_count()
        if node.func_name is not None:
            print('\t' * self.tab_count, "Name => ", node.func_name.lexeme)
        if node.func_type is not None:
            node.func_type.accept(self)
        self.dec_tab_count()
    
    def visit_function_decl_with_params_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "Function Declaration => ")
        self.inc_tab_count()
        if node.func_name is not None:
            print('\t' * self.tab_count, "Name => ", node.func_name.lexeme)
        print('\t' * self.tab_count, "Params => ")
        self.inc_tab_count()
        for param_name, param_type in node.params.items():
            param_name.accept(self)
            param_type.accept(self)
            print()
        self.dec_tab_count()
        if node.func_type is not None:
            node.func_type.accept(self)
        self.dec_tab_count()
    
    def visit_function_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "Function => ")
        self.inc_tab_count() 
        if node.func_decl is not None:
            node.func_decl.accept(self)
        else:
            print('\t' * self.tab_count, "Hello Function!")
        print('\t' * self.tab_count, "Function Body:")
        self.inc_tab_count()
        self.visit_block_node(node.func_body)
        self.dec_tab_count()
    
    def visit_function_call_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "Function Call => ")
        self.inc_tab_count() 
        if node.func_name is not None:
            node.func_name.accept(self)
        self.dec_tab_count()
    
    def visit_print_node(self, lex_node):
        self.node_count += 1
        if isinstance(lex_node.expr, ast.ASTVariableNode):
            print('\t' * self.tab_count, "Print => ", lex_node.expr.lexeme)
        elif isinstance(lex_node.expr, ast.ASTArrayElementNode):
            print('\t' * self.tab_count, "Print => ", lex_node.expr.arr_node.values[int(lex_node.expr.index.value)].value)
        else:
            print('\t' * self.tab_count, "Print => ", lex_node.expr.value)
    
    def visit_clear_node(self, clear_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Clear => ")
        self.inc_tab_count()
        if isinstance(clear_node.expr, ast.ASTColourNode):
            self.visit_colour_node(clear_node.expr)
        elif isinstance(clear_node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(clear_node.expr)
        elif isinstance(clear_node.expr, ast.ASTVariableNode):
            self.visit_variable_node(clear_node.expr)
        self.dec_tab_count()
    
    def visit_parenthesis_node(self, brac_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Brackets => ", brac_node.expression)
    
    def visit_delay_node(self, delay_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Delay => ", delay_node.expr.value, " milliseconds")
    
    def visit_write_node(self, wr_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Write => ")
        self.inc_tab_count()
        self.visit_colour_node(wr_node.colour)
        self.visit_integer_node(wr_node.yPos)
        self.visit_integer_node(wr_node.xPos)
        self.dec_tab_count()
    
    def visit_write_box_node(self, wr_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Write Box => ")
        self.inc_tab_count()
        wr_node.colour.accept(self)
        wr_node.yPos.accept(self)
        wr_node.xPos.accept(self)
        wr_node.height.accept(self)
        wr_node.width.accept(self)
        self.dec_tab_count()
    
    def visit_random_node(self, delay_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Random => ", delay_node.expr.value)
        self.inc_tab_count()
        print('\t' * self.tab_count, "From: ", 0)
        print('\t' * self.tab_count, "To: ", delay_node.expr.value)
        self.dec_tab_count()

    def visit_condition_node(self, cond_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Condition => ",)
        self.inc_tab_count()
        if (isinstance(cond_node.id, ast.ASTVariableNode)):
            cond_node.id.accept(self)
        else:
            cond_node.id.accept(self)
        
        cond_node.relOp.accept(self)

        if (isinstance(cond_node.expr, ast.ASTVariableNode)):
            cond_node.expr.accept(self)
        else:
            cond_node.expr.accept(self)
        self.dec_tab_count()

    def visit_block_node(self, block_node):
        self.node_count += 1
        print('\t' * self.tab_count, "New Block => ")
        self.inc_tab_count()
        
        for st in block_node.stmts:
            st.accept(self)
        
        self.dec_tab_count()

    def visit_program_node(self, prog_node):
        self.node_count += 1
        print('\t' * self.tab_count, "New Block => ")
        self.inc_tab_count()
        
        for st in prog_node.stmts:
            st.accept(self)
        
        self.dec_tab_count()
    
    def visit_end_node(self, node):
        print("END of AST")


class SemanticAnalyser(ASTVisitor):
    def __init__(self):
        self.name = "Semantic Analysis Visitor"
        self.error_count = 0
        self.variable_table = {}  # To keep track of variable indices and types
        self.array_table = {}
        self.function_table = {}
        self.node_count = 0
        self.tab_count = 0

    def check_semantics(self, ast_root):
        if isinstance(ast_root, ast.ASTAssignmentNode):
            self.visit_assignment_node(ast_root)
        if isinstance(ast_root, ast.ASTAssignmentArrayNode):
            self.visit_assignment_array_node(ast_root)
        elif isinstance(ast_root, ast.ASTVariableNode):
            self.visit_variable_node(ast_root)
        elif isinstance(ast_root, ast.ASTConditionNode):
            self.visit_condition_node(ast_root)
        elif isinstance(ast_root, ast.ASTVarDeclNode):
            self.visit_var_decl_node(ast_root)
        elif isinstance(ast_root, ast.ASTVarDeclArrayNode):
            self.visit_var_decl_array_node(ast_root)
        elif isinstance(ast_root, ast.ASTIfNode):
            self.visit_if_node(ast_root)
        elif isinstance(ast_root, ast.ASTIfElseNode):
            self.visit_if_else_node(ast_root)
        elif isinstance(ast_root, ast.ASTForNode):
            self.visit_for_node(ast_root)
        elif isinstance(ast_root, ast.ASTWhileNode):
            self.visit_while_node(ast_root)
        elif isinstance(ast_root, ast.ASTPrintNode):
            self.visit_print_node(ast_root)
        elif isinstance(ast_root, ast.ASTDelayNode):
            self.visit_delay_node(ast_root)
        elif isinstance(ast_root, ast.ASTClearNode):
            self.visit_clear_node(ast_root)
        elif isinstance(ast_root, ast.ASTWriteNode):
            self.visit_write_node(ast_root)
        elif isinstance(ast_root, ast.ASTWriteBoxNode):
            self.visit_write_box_node(ast_root)
        elif isinstance(ast_root, ast.ASTRandomNode):
            self.visit_random_node(ast_root)
        elif isinstance(ast_root, ast.ASTReturnNode):
            self.visit_return_node(ast_root)
        elif isinstance(ast_root, ast.ASTFunctionNode):
            self.visit_function_node(ast_root)
        elif isinstance(ast_root, ast.ASTFunctionCallNode):
            self.visit_function_call_node(ast_root)
        elif isinstance(ast_root, ast.ASTEndNode):
            self.visit_end_node(ast_root)

    def visit_integer_node(self, node):
        if not isinstance(node, ast.ASTIntegerNode):
            print(f"Semantic Error: Value {node.value} not of type integer!")
            self.error_count += 1

    def visit_bool_node(self, node):
        if not isinstance(node, ast.ASTBoolNode):
            print(f"Semantic Error: Value {node.value} not of type boolean!")
            self.error_count += 1

    def visit_float_node(self, node):
        if not isinstance(node, ast.ASTFloatNode):
            print(f"Semantic Error: Value {node.value} not of type float!")
            self.error_count += 1

    def visit_colour_node(self, node):
        if not isinstance(node, ast.ASTColourNode):
            print(f"Semantic Error: Value {node.value} not of type colour!")
            self.error_count += 1
    
    def visit_width_node(self, node):
        if not isinstance(node, ast.ASTWidthNode):
            print(f"Semantic Error: Not width keyword!")
            self.error_count += 1
    
    def visit_height_node(self, node):
        if not isinstance(node, ast.ASTHeightNode):
            print(f"Semantic Error: Not height keyword!")
            self.error_count += 1
    
    def visit_array_node(self, node):
        if not isinstance(node, ast.ASTArrayNode):
            print(f"Semantic Error: Not an array!")
            self.error_count += 1
        else:
            for val in node.values:
                val.accept(self)
            
            self.visit_integer_node(node.length)

    def visit_variable_node(self, node):
        if not isinstance(node, ast.ASTVariableNode):
            print(f"Semantic Error: Variable {node.lexeme} not defined!")
            self.error_count += 1

    def visit_parenthesis_node(self, node):
        self.visit(node.expression)

    def visit_operator_node(self, node):
        if not isinstance(node, ast.ASTOperatorNode):
            print(f"Semantic Error: Operator {node} not defined!")
            self.error_count += 1

    def visit_binary_operator_node(self, node):
        if not isinstance(node, ast.ASTBinaryOpNode):
            print(f"Semantic Error: Binary operator {node.binOp} not defined!")
            self.error_count += 1
    
    def visit_return_node(self, node):
        if isinstance(node.val, ast.ASTIntegerNode):
            self.visit_integer_node(node.val)
        elif isinstance(node.val, ast.ASTBoolNode):
            self.visit_bool_node(node.val)
        elif isinstance(node.val, ast.ASTFloatNode):
            self.visit_float_node(node.val)
        elif isinstance(node.val, ast.ASTColourNode):
            self.visit_colour_node(node.val)
        elif isinstance(node.val, ast.ASTWidthNode):
            self.visit_width_node(node.val)
        elif isinstance(node.val, ast.ASTHeightNode):
            self.visit_height_node(node.val)
        elif isinstance(node.val, ast.ASTVariableNode):
            self.visit_variable_node(node.val)

    def visit_assignment_node(self, node):
        if not isinstance(node.id, ast.ASTVariableNode):
            print(f"Semantic Error: Variable {node.id.lexeme} not defined!")
            self.error_count += 1
        if node.id.lexeme not in self.variable_table.keys():
            print(f"Semantic Error: Variable {node.id.lexeme} not declared!")
            self.error_count += 1

        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            self.visit_height_node(node.expr)
        elif isinstance(node.expr, ast.ASTArrayElementNode):
            self.visit_array_element_node(node.expr)
        elif isinstance(node.expr, ast.ASTModExpressionNode):
            node.expr.accept(self)

    def visit_assignment_array_node(self, node):
        if not isinstance(node.id, ast.ASTVariableNode):
            print(f"Semantic Error: Variable {node.id.lexeme} not defined!")
            self.error_count += 1
        
        if node.id.lexeme not in self.variable_table.keys():
            print(f"Semantic Error: Variable {node.id.lexeme} not declared!")
            self.error_count += 1
        
        if node.id.lexeme not in self.array_table.keys():
            print(f"Semantic Error: Array variable {node.id.lexeme} not declared as an array!")
            self.error_count += 1
        
        # Push the initial value to the stack and store it
        for val in node.expr.values:
            if isinstance(val, ast.ASTIntegerNode):
                self.visit_integer_node(val)
            elif isinstance(val, ast.ASTBoolNode):
                self.visit_bool_node(val)
            elif isinstance(val, ast.ASTFloatNode):
                self.visit_float_node(val)
            elif isinstance(val, ast.ASTColourNode):
                self.visit_colour_node(val)
            elif isinstance(val, ast.ASTRandomNode):
                self.visit_random_node(val)
            elif isinstance(val, ast.ASTWidthNode):
                self.visit_width_node(val)
            elif isinstance(val, ast.ASTHeightNode):
                self.visit_height_node(val)
    
    def visit_var_decl_node(self, node):
        if not isinstance(node.id, ast.ASTVariableNode):
            print(f"Semantic Error: Variable {node.id.lexeme} not defined!")
            self.error_count += 1
        
        if not isinstance(node.type, ast.ASTDataTypeNode):
            print(f"Semantic Error: Datatype {node.type.type} not defined!")
            self.error_count += 1
        
        var_name = node.id.lexeme

        if var_name in self.variable_table.keys():
            print(f"Semantic Error: Variable {node.id.lexeme} already defined!")
            self.error_count += 1
        else:
            var_index = len(self.variable_table)  # Assign a new index for the variable
            self.variable_table[var_name] = var_index
        
        # Push the initial value to the stack and store it
        if isinstance(node.expr, ast.ASTIntegerNode):
            if node.type.type != "int":
                self.error_count += 1
                print(f"Semantic Error: Value {node.expr.value} in variable {node.id.lexeme} not set to {node.type.type}!")
            else:
                self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            if node.type.type != "bool":
                self.error_count += 1
                print(f"Semantic Error: Value {node.expr.value} in variable {node.id.lexeme} not set to {node.type.type}!")
            else:
                self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            if node.type.type != "float":
                self.error_count += 1
                print(f"Semantic Error: Value {node.expr.value} in variable {node.id.lexeme} not set to {node.type.type}!")
            else:
                self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            if node.type.type != "colour":
                self.error_count += 1
                print(f"Semantic Error: Value {node.expr.value} in variable {node.id.lexeme} not set to {node.type.type}!")
            else:
                self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTRandomNode):
            self.visit_random_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            if node.type.type != "int":
                self.error_count += 1
                print(f"Semantic Error: Value {node.expr.value} in variable {node.id.lexeme} not set to {node.type.type}!")
            else:
                self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            if node.type.type != "int":
                self.error_count += 1
                print(f"Semantic Error: Value {node.expr.value} in variable {node.id.lexeme} not set to {node.type.type}!")
            else:
                self.visit_height_node(node.expr)
        elif isinstance(node.expr, ast.ASTArrayElementNode):
            self.visit_array_element_node(node.expr)
    
    def visit_var_decl_array_node(self, node):
        if not isinstance(node.id, ast.ASTVariableNode):
            print(f"Semantic Error: Variable {node.id.lexeme} not defined!")
            self.error_count += 1
        
        if not isinstance(node.type, ast.ASTDataTypeNode):
            print(f"Semantic Error: Datatype {node.type.type} not defined!")
            self.error_count += 1
        
        var_name = node.id.lexeme

        if var_name in self.variable_table.keys():
            print(f"Semantic Error: Variable {node.id.lexeme} already defined!")
            self.error_count += 1
        else:
            var_index = len(self.variable_table)  # Assign a new index for the variable
            self.variable_table[var_name] = var_index
            arr_index = len(self.array_table)  # Assign a new index for the variable
            self.array_table[var_name] = arr_index
        
        # Push the initial value to the stack and store it
        for val in node.expr.values:
            if isinstance(val, ast.ASTIntegerNode):
                if node.type.type != "int":
                    self.error_count += 1
                    print(f"Semantic Error: Value {val.value} in variable {node.id.lexeme} not set to {node.type.type}!")
                else:
                    self.visit_integer_node(val)
            elif isinstance(val, ast.ASTBoolNode):
                if node.type.type != "bool":
                    self.error_count += 1
                    print(f"Semantic Error: Value {val.value} in variable {node.id.lexeme} not set to {node.type.type}!")
                else:
                    self.visit_bool_node(val)
            elif isinstance(val, ast.ASTFloatNode):
                if node.type.type != "float":
                    self.error_count += 1
                    print(f"Semantic Error: Value {val.value} in variable {node.id.lexeme} not set to {node.type.type}!")
                else:
                    self.visit_float_node(val)
            elif isinstance(val, ast.ASTColourNode):
                if node.type.type != "colour":
                    self.error_count += 1
                    print(f"Semantic Error: Value {val.value} in variable {node.id.lexeme} not set to {node.type.type}!")
                else:
                    self.visit_colour_node(val)
            elif isinstance(val, ast.ASTRandomNode):
                self.visit_random_node(val)
            elif isinstance(val, ast.ASTWidthNode):
                if node.type.type != "int":
                    self.error_count += 1
                    print(f"Semantic Error: Value {val.value} in variable {node.id.lexeme} not set to {node.type.type}!")
                else:
                    self.visit_width_node(val)
            elif isinstance(val, ast.ASTHeightNode):
                if node.type.type != "int":
                    self.error_count += 1
                    print(f"Semantic Error: Value {val.value} in variable {node.id.lexeme} not set to {node.type.type}!")
                else:
                    self.visit_height_node(val)
        
    def visit_mod_expr_node(self, node):
        node.id.accept(self)
        node.operator.accept(self)
        node.expr.accept(self)
    
    def visit_array_element_node(self, node):
        node.index.accept(self)
        node.arr_node.accept(self)
    
    def visit_condition_node(self, node):
        if isinstance(node.id, ast.ASTIntegerNode):
            self.visit_integer_node(node.id)
        elif isinstance(node.id, ast.ASTBoolNode):
            self.visit_bool_node(node.id)
        elif isinstance(node.id, ast.ASTFloatNode):
            self.visit_float_node(node.id)
        elif isinstance(node.id, ast.ASTColourNode):
            self.visit_colour_node(node.id)
        elif isinstance(node.id, ast.ASTVariableNode):
            self.visit_variable_node(node.id)
        elif isinstance(node.id, ast.ASTWidthNode):
            self.visit_width_node(node.id)
        elif isinstance(node.id, ast.ASTHeightNode):
            self.visit_height_node(node.id)

        self.visit_binary_operator_node(node.relOp)

        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTVariableNode):
            self.visit_variable_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            self.visit_height_node(node.expr)

    def visit_if_node(self, node):
        self.visit_condition_node(node.condition)
        self.visit_block_node(node.if_body)

    def visit_if_else_node(self, node):
        self.visit_condition_node(node.condition)
        self.visit_block_node(node.if_body)
        self.visit_block_node(node.else_body)
    
    def visit_for_node(self, node):
        self.visit_var_decl_node(node.var_init)
        self.visit_condition_node(node.condition)
        self.visit_assignment_node(node.var_inc)
        self.visit_block_node(node.for_body)
    
    def visit_while_node(self, node):
        self.visit_condition_node(node.condition)
        self.visit_block_node(node.while_body)
    
    def visit_function_node(self, node):
        if not isinstance(node.func_decl, ast.ASTFunctionDeclNode) or not isinstance(node.func_decl, ast.ASTFunctionDeclNodeWithParams):
            print(f"Semantic Error: Function {node.func_decl.func_name.lexeme} not defined!")
            self.error_count += 1

        func_name = node.func_decl.func_name.lexeme
        var_index = len(self.function_table)  # Assign a new index for the variable
        self.function_table[func_name] = var_index
        
        if isinstance(node.func_decl, ast.ASTFunctionDeclNodeWithParams):
            for param in node.func_decl.params:
                param.accept(self)
        
        if not isinstance(node.func_body, ast.ASTBlockNode):
            print(f"Semantic Error: Function body not in block!")
            self.error_count += 1
    
    def visit_function_call_node(self, node):
        if not isinstance(node.func_decl, ast.ASTFunctionDeclNode) or not isinstance(node.func_decl, ast.ASTFunctionDeclNodeWithParams):
            print(f"Semantic Error: Function {node.func_decl.func_name.lexeme} not defined!")
            self.error_count += 1
        
        if node.func_decl.func_name.lexeme not in self.function_table:
            print(f"Semantic Error: Function {node.func_decl.func_name.lexeme} not declared!")
            self.error_count += 1

    def visit_print_node(self, node):
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTVariableNode):
            self.visit_variable_node(node.expr)

    def visit_delay_node(self, node):
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
    
    def visit_clear_node(self, node):
        if isinstance(node.expr, ast.ASTVariableNode):
            self.visit_variable_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        else:
            print("Semantic Error: Can only clear colour variables or colour values!")
    
    def visit_write_node(self, node):
        if isinstance(node.colour, ast.ASTColourNode):
            self.visit_colour_node(node.colour)
        elif isinstance(node.colour, ast.ASTIntegerNode):
            self.visit_integer_node(node.colour)
        elif isinstance(node.colour, ast.ASTVariableNode):
            self.visit_variable_node(node.colour)

        if isinstance(node.yPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTVariableNode):
            self.visit_variable_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTHeightNode):
            self.visit_height_node(node.yPos)

        if isinstance(node.xPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTVariableNode):
            self.visit_variable_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTWidthNode):
            self.visit_width_node(node.xPos)
    
    def visit_write_box_node(self, node):
        if isinstance(node.colour, ast.ASTColourNode):
            self.visit_colour_node(node.colour)
        elif isinstance(node.colour, ast.ASTIntegerNode):
            self.visit_integer_node(node.colour)
        elif isinstance(node.colour, ast.ASTVariableNode):
            self.visit_variable_node(node.colour)

        if isinstance(node.yPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTVariableNode):
            self.visit_variable_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTHeightNode):
            self.visit_height_node(node.yPos)

        if isinstance(node.xPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTVariableNode):
            self.visit_variable_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTWidthNode):
            self.visit_width_node(node.xPos)
        
        if isinstance(node.height, ast.ASTIntegerNode):
            self.visit_integer_node(node.height)
        elif isinstance(node.height, ast.ASTVariableNode):
            self.visit_variable_node(node.height)
        elif isinstance(node.height, ast.ASTHeightNode):
            self.visit_height_node(node.height)

        if isinstance(node.width, ast.ASTIntegerNode):
            self.visit_integer_node(node.width)
        elif isinstance(node.width, ast.ASTVariableNode):
            self.visit_variable_node(node.width)
        elif isinstance(node.width, ast.ASTWidthNode):
            self.visit_width_node(node.width)
    
    def visit_random_node(self, node):
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            self.visit_height_node(node.expr)

    def visit_block_node(self, node):
        for stmt in node.stmts:
            self.check_semantics(stmt)
    
    def visit_program_node(self, node):
        for stmt in node.stmts:
            self.check_semantics(stmt)
    
    def visit_end_node(self, node):
        print(f'Number of semantic errors: {self.error_count}')


class PArIRCodeGenerator(ASTVisitor):
    def __init__(self):
        self.name = "Code Generation Visitor"
        self.code = ".main\n"
        self.variable_table = {}  # To keep track of variable indices and types
        self.array_table = {}
        self.array_vals_table = {}
        self.array_len_table = {}
        self.function_table = {}
        self.labels = {}  # To store label positions
        self.label_count = 0  # To generate unique labels

    def intialise_vars(self, var_count):
        if (var_count > 0):
            self.code += f"push {var_count}\noframe\n"
    
    def get_new_label(self):
        label = f"label{self.label_count}"
        self.label_count += 1
        return label
    
    def replace_placeholders(self):
        lines = self.code.splitlines()
        for i, line in enumerate(lines):
            if '@' in line:
                label = line.split('@')[1]
                target_line = self.labels[label]
                offset = target_line - i  # Calculate offset from current line
                lines[i] = line.replace(f"@{label}@", f"#PC{offset:+d}")
        self.code = ""
        for line in lines:
            self.code += line+"\n"

    def generate_code(self, ast_root):
        if isinstance(ast_root, ast.ASTAssignmentNode):
            self.visit_assignment_node(ast_root)
        if isinstance(ast_root, ast.ASTAssignmentArrayNode):
            self.visit_assignment_array_node(ast_root)
        elif isinstance(ast_root, ast.ASTVariableNode):
            self.visit_variable_node(ast_root)
        elif isinstance(ast_root, ast.ASTConditionNode):
            self.visit_condition_node(ast_root)
        elif isinstance(ast_root, ast.ASTVarDeclNode):
            self.visit_var_decl_node(ast_root)
        elif isinstance(ast_root, ast.ASTVarDeclArrayNode):
            self.visit_var_decl_array_node(ast_root)
        elif isinstance(ast_root, ast.ASTIfNode):
            self.visit_if_node(ast_root)
        elif isinstance(ast_root, ast.ASTIfElseNode):
            self.visit_if_else_node(ast_root)
        elif isinstance(ast_root, ast.ASTForNode):
            self.visit_for_node(ast_root)
        elif isinstance(ast_root, ast.ASTWhileNode):
            self.visit_while_node(ast_root)
        elif isinstance(ast_root, ast.ASTPrintNode):
            self.visit_print_node(ast_root)
        elif isinstance(ast_root, ast.ASTDelayNode):
            self.visit_delay_node(ast_root)
        elif isinstance(ast_root, ast.ASTClearNode):
            self.visit_clear_node(ast_root)
        elif isinstance(ast_root, ast.ASTWriteNode):
            self.visit_write_node(ast_root)
        elif isinstance(ast_root, ast.ASTWriteBoxNode):
            self.visit_write_box_node(ast_root)
        elif isinstance(ast_root, ast.ASTRandomNode):
            self.visit_random_node(ast_root)
        elif isinstance(ast_root, ast.ASTReturnNode):
            self.visit_return_node(ast_root)
        elif isinstance(ast_root, ast.ASTFunctionNode):
            self.visit_function_node(ast_root)
        elif isinstance(ast_root, ast.ASTFunctionCallNode):
            self.visit_function_call_node(ast_root)
        elif isinstance(ast_root, ast.ASTEndNode):
            self.visit_end_node(ast_root)
            self.replace_placeholders()
        return self.code

    def visit_integer_node(self, node):
        if (not isinstance(node.value, ast.ASTWidthNode) or not isinstance(node.value, ast.ASTHeightNode)):
            self.code += f"push {int(node.value)}\n"
        elif isinstance(node.value, ast.ASTWidthNode):
            self.visit_width_node
        elif isinstance(node.value, ast.ASTHeightNode):
            self.visit_height_node

    def visit_bool_node(self, node):
        self.code += f"push {node.value}\n"

    def visit_float_node(self, node):
        if (not isinstance(node.value, ast.ASTWidthNode) or not isinstance(node.value, ast.ASTHeightNode)):
            self.code += f"push {float(node.value)}\n"
        elif isinstance(node.value, ast.ASTWidthNode):
            self.visit_width_node
        elif isinstance(node.value, ast.ASTHeightNode):
            self.visit_height_node

    def visit_colour_node(self, node):
        self.code += f"push {node.value}\n"
    
    def visit_width_node(self, node):
        self.code += f"width\n"
    
    def visit_height_node(self, node):
        self.code += f"height\n"
    
    def visit_array_node(self, node):
        for val in node.values:
            val.accept(self)

    def visit_variable_node(self, node):
        if node.lexeme in self.variable_table:
            var_index = self.variable_table[node.lexeme]
            if node.lexeme not in self.array_table:
                self.code += f"push [{str(var_index)}:0]\n"
            else:
                arr_index = self.array_table[node.lexeme]
                arr_length = self.array_len_table[node.lexeme]
                self.code += f"push {str(arr_length.value)}\npusha [{str(var_index)}:0]\npush {str(arr_length.value)}\n"
        else:
            self.code += f"push {str(node.lexeme)}\n"

    def visit_parenthesis_node(self, node):
        self.visit(node.expression)

    def visit_operator_node(self, node):
        if (node.operator == "+"):
            self.code += "add\n"
        elif (node.operator == "-"):
            self.code += "sub\n"
        elif (node.operator == "*"):
            self.code += "mul\n"
        elif (node.operator == "/"):
            self.code += "div\n"

    def visit_binary_operator_node(self, node):
        if node.binOp == "<":
            self.code += f"lt\n"
        elif node.binOp == "<=":
            self.code += f"le\n"
        elif node.binOp == ">":
            self.code += f"gt\n"
        elif node.binOp == ">=":
            self.code += f"ge\n"
        elif node.binOp == "==":
            self.code += f"eq\n"
        elif node.binOp == "!=":
            self.code += f"eq\nnot\n"
    
    def visit_return_node(self, node):
        if isinstance(node.val, ast.ASTIntegerNode):
            self.visit_integer_node(node.val)
        elif isinstance(node.val, ast.ASTBoolNode):
            self.visit_bool_node(node.val)
        elif isinstance(node.val, ast.ASTFloatNode):
            self.visit_float_node(node.val)
        elif isinstance(node.val, ast.ASTColourNode):
            self.visit_colour_node(node.val)
        elif isinstance(node.val, ast.ASTWidthNode):
            self.visit_width_node(node.val)
        elif isinstance(node.val, ast.ASTHeightNode):
            self.visit_height_node(node.val)
        elif isinstance(node.val, ast.ASTVariableNode):
            self.visit_variable_node(node.val)
        self.code += f"ret\n"

    def visit_assignment_node(self, node):
        var_name = node.id.lexeme
        var_index = 0
        if var_name in self.variable_table.keys():
            var_index = self.variable_table[var_name]

        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            self.visit_height_node(node.expr)
        elif isinstance(node.expr, ast.ASTArrayElementNode):
            self.visit_array_element_node(node.expr)
        elif isinstance(node.expr, ast.ASTModExpressionNode):
            node.expr.expr.accept(self)
            node.expr.id.accept(self)
            node.expr.operator.accept(self)
        #self.code += f"st\n"
        self.code += f"push {var_index}\npush 0\nst\n"  # Store the value at index var_index in the current frame
    
    def visit_assignment_array_node(self, node):
        var_name = node.id.lexeme
        var_index = self.variable_table[var_name]
        var_arr_index = self.array_table[var_name]
        arr_length = node.expr.length
        self.array_len_table[var_name] = arr_length
        
        # Push the initial value to the stack and store it
        for val in node.expr.values:
            if isinstance(val, ast.ASTIntegerNode):
                self.visit_integer_node(val)
            elif isinstance(val, ast.ASTBoolNode):
                self.visit_bool_node(val)
            elif isinstance(val, ast.ASTFloatNode):
                self.visit_float_node(val)
            elif isinstance(val, ast.ASTColourNode):
                self.visit_colour_node(val)
            elif isinstance(val, ast.ASTRandomNode):
                self.visit_random_node(val)
            elif isinstance(val, ast.ASTWidthNode):
                self.visit_width_node(val)
            elif isinstance(val, ast.ASTHeightNode):
                self.visit_height_node(val)
        self.code += f"push {node.expr.length.value}\npush {var_index}\npush 0\nsta\n"  # Store the value at index var_index in the current frame
    
    def visit_var_decl_node(self, node):
        var_name = node.id.lexeme
        var_index = len(self.variable_table)  # Assign a new index for the variable
        self.variable_table[var_name] = var_index
        
        # Push the initial value to the stack and store it
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTRandomNode):
            self.visit_random_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            self.visit_height_node(node.expr)
        elif isinstance(node.expr, ast.ASTArrayElementNode):
            self.visit_array_element_node(node.expr)
        elif isinstance(node.expr, ast.ASTModExpressionNode):
            node.expr.expr.accept(self)
            node.expr.id.accept(self)
            node.expr.operator.accept(self)
        self.code += f"push {var_index}\npush 0\nst\n"  # Store the value at index var_index in the current frame
    
    def visit_var_decl_array_node(self, node):
        var_name = node.id.lexeme
        var_index = len(self.variable_table)  # Assign a new index for the variable
        self.variable_table[var_name] = var_index
        var_arr_index = len(self.array_table)  # Assign a new index for the variable
        self.array_table[var_name] = var_arr_index
        self.array_vals_table[var_name] = node.expr.values
        arr_length = node.expr.length
        self.array_len_table[var_name] = arr_length
        
        # Push the initial value to the stack and store it
        for val in node.expr.values:
            if isinstance(val, ast.ASTIntegerNode):
                self.visit_integer_node(val)
            elif isinstance(val, ast.ASTBoolNode):
                self.visit_bool_node(val)
            elif isinstance(val, ast.ASTFloatNode):
                self.visit_float_node(val)
            elif isinstance(val, ast.ASTColourNode):
                self.visit_colour_node(val)
            elif isinstance(val, ast.ASTRandomNode):
                self.visit_random_node(val)
            elif isinstance(val, ast.ASTWidthNode):
                self.visit_width_node(val)
            elif isinstance(val, ast.ASTHeightNode):
                self.visit_height_node(val)
        self.code += f"push {node.expr.length.value}\npush {var_index}\npush 0\nsta\n"  # Store the value at index var_index in the current frame
    
    def visit_array_element_node(self, node):
        # Find the keys corresponding to the target value
        var_name = [key for key, value in self.array_vals_table.items() if value == node.arr_node.values]
        var_index = self.variable_table[var_name[0]]
        arr_length = self.array_len_table[var_name[0]]
        node.index.accept(self)
        self.code += f"push 1\nadd\npush {arr_length.value}\nsub\npush +[{str(var_index)}:0]\n"  # Store the value at index var_index in the current frame
    
    def visit_mod_expr_node(self, node):
        node.id.accept(self)
        node.expr.accept(self)
        node.binOp.accept(self)

    def visit_condition_node(self, node):
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTVariableNode):
            self.visit_variable_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            self.visit_height_node(node.expr)

        if isinstance(node.id, ast.ASTIntegerNode):
            self.visit_integer_node(node.id)
        elif isinstance(node.id, ast.ASTBoolNode):
            self.visit_bool_node(node.id)
        elif isinstance(node.id, ast.ASTFloatNode):
            self.visit_float_node(node.id)
        elif isinstance(node.id, ast.ASTColourNode):
            self.visit_colour_node(node.id)
        elif isinstance(node.id, ast.ASTVariableNode):
            self.visit_variable_node(node.id)
        elif isinstance(node.id, ast.ASTWidthNode):
            self.visit_width_node(node.id)
        elif isinstance(node.id, ast.ASTHeightNode):
            self.visit_height_node(node.id)
        self.visit_binary_operator_node(node.relOp)

    def visit_if_node(self, node):
        self.visit_condition_node(node.condition)
        true_label = self.get_new_label()
        end_label = self.get_new_label()
        self.code += f"push @{true_label}@\ncjmp\npush @{end_label}@\njmp\n"
        self.labels[true_label] = len(self.code.splitlines())
        self.visit_block_node(node.if_body)
        self.labels[end_label] = len(self.code.splitlines())

    def visit_if_else_node(self, node):
        self.visit_condition_node(node.condition)
        true_label = self.get_new_label()
        else_label = self.get_new_label()
        end_label = self.get_new_label()
        self.code += f"push @{true_label}@\ncjmp\npush @{else_label}@\njmp\n"
        self.labels[true_label] = len(self.code.splitlines())
        self.visit_block_node(node.if_body)
        self.code += f"push @{end_label}@\njmp\n"
        self.labels[else_label] = len(self.code.splitlines())
        self.visit_block_node(node.else_body)
        self.labels[end_label] = len(self.code.splitlines())
    
    def visit_for_node(self, node):
        start_label = self.get_new_label()
        body_label = self.get_new_label()
        end_label = self.get_new_label()

        self.visit_var_decl_node(node.var_init)
        self.labels[start_label] = len(self.code.splitlines())
        self.visit_condition_node(node.condition)
        self.code += f"push @{body_label}@\ncjmp\npush @{end_label}@\njmp\n"
        self.labels[body_label] = len(self.code.splitlines())
        self.visit_block_node(node.for_body)
        self.visit_assignment_node(node.var_inc)
        self.code += f"push @{start_label}@\njmp\n"
        self.labels[end_label] = len(self.code.splitlines())
    
    def visit_while_node(self, node):
        start_label = self.get_new_label()
        body_label = self.get_new_label()
        end_label = self.get_new_label()

        self.labels[start_label] = len(self.code.splitlines())
        self.visit_condition_node(node.condition)
        self.code += f"push @{body_label}@\ncjmp\npush @{end_label}@\njmp\n"
        self.labels[body_label] = len(self.code.splitlines())
        self.visit_block_node(node.while_body)
        self.code += f"push @{start_label}@\njmp\n"
        self.labels[end_label] = len(self.code.splitlines())
    
    def visit_function_node(self, node):
        func_name = node.func_decl.func_name.lexeme
        var_index = len(self.function_table)  # Assign a new index for the variable
        self.function_table[func_name] = var_index

        self.code += f".{func_name}\n"
        self.visit_block_node(node.func_body)
    
    def visit_function_call_node(self, node):
        self.code += f"push 1\n"
        self.code += f"push .{node.func_name.lexeme}\ncall\n"

    def visit_print_node(self, node):
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTVariableNode):
            self.visit_variable_node(node.expr)
        elif isinstance(node.expr, ast.ASTArrayElementNode):
            self.visit_array_element_node(node.expr)
        
        if isinstance(node.expr, ast.ASTVariableNode):
            if node.expr.lexeme not in self.array_table:
                self.code += "print\n"
            else:
                self.code += "printa\n"
        else:
            self.code += "print\n"
    
    def visit_delay_node(self, node):
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        self.code += f"delay\n"
    
    def visit_clear_node(self, node):
        if isinstance(node.expr, ast.ASTVariableNode):
            self.visit_variable_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        self.code += f"clear\n"
    
    def visit_write_node(self, node):
        if isinstance(node.colour, ast.ASTColourNode):
            self.visit_colour_node(node.colour)
        elif isinstance(node.colour, ast.ASTIntegerNode):
            self.visit_integer_node(node.colour)
        elif isinstance(node.colour, ast.ASTVariableNode):
            self.visit_variable_node(node.colour)

        if isinstance(node.yPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTVariableNode):
            self.visit_variable_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTHeightNode):
            self.visit_height_node(node.yPos)

        if isinstance(node.xPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTVariableNode):
            self.visit_variable_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTWidthNode):
            self.visit_width_node(node.xPos)
        self.code += f"write\n"
    
    def visit_write_box_node(self, node):
        if isinstance(node.colour, ast.ASTColourNode):
            self.visit_colour_node(node.colour)
        elif isinstance(node.colour, ast.ASTIntegerNode):
            self.visit_integer_node(node.colour)
        elif isinstance(node.colour, ast.ASTVariableNode):
            self.visit_variable_node(node.colour)

        if isinstance(node.yPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTVariableNode):
            self.visit_variable_node(node.yPos)
        elif isinstance(node.yPos, ast.ASTHeightNode):
            self.visit_height_node(node.yPos)

        if isinstance(node.xPos, ast.ASTIntegerNode):
            self.visit_integer_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTVariableNode):
            self.visit_variable_node(node.xPos)
        elif isinstance(node.xPos, ast.ASTWidthNode):
            self.visit_width_node(node.xPos)
        
        if isinstance(node.height, ast.ASTIntegerNode):
            self.visit_integer_node(node.height)
        elif isinstance(node.height, ast.ASTVariableNode):
            self.visit_variable_node(node.height)
        elif isinstance(node.height, ast.ASTHeightNode):
            self.visit_height_node(node.height)

        if isinstance(node.width, ast.ASTIntegerNode):
            self.visit_integer_node(node.width)
        elif isinstance(node.width, ast.ASTVariableNode):
            self.visit_variable_node(node.width)
        elif isinstance(node.width, ast.ASTWidthNode):
            self.visit_width_node(node.width)
        self.code += f"writebox\n"
    
    def visit_random_node(self, node):
        if isinstance(node.expr, ast.ASTIntegerNode):
            self.visit_integer_node(node.expr)
        elif isinstance(node.expr, ast.ASTBoolNode):
            self.visit_bool_node(node.expr)
        elif isinstance(node.expr, ast.ASTFloatNode):
            self.visit_float_node(node.expr)
        elif isinstance(node.expr, ast.ASTColourNode):
            self.visit_colour_node(node.expr)
        elif isinstance(node.expr, ast.ASTWidthNode):
            self.visit_width_node(node.expr)
        elif isinstance(node.expr, ast.ASTHeightNode):
            self.visit_height_node(node.expr)
        self.code += f"irnd\n"

    def visit_block_node(self, node):
        for stmt in node.stmts:
            self.generate_code(stmt)
    
    def visit_program_node(self, node):
        for stmt in node.stmts:
            self.generate_code(stmt)
    
    def visit_end_node(self, node):
        self.code += "halt\n"


#Create a print visitor instance
print_visitor = PrintNodesVisitor()

#assume root node the AST assignment node .... 
#x=23
print("Building AST for assigment statement x=#00ff00;")
assignment_lhs = ast.ASTVariableNode("x")
assignment_rhs = ast.ASTColourNode("#00ff00")
root = ast.ASTAssignmentNode(assignment_lhs, assignment_rhs)
root.accept(print_visitor)
print("Node Count => ", print_visitor.node_count)
print("----")

code_generator_2 = PArIRCodeGenerator()
code_2 = code_generator_2.generate_code(root)
print("\nParIR Code:")
print(code_2)

print("Building AST for condition x123 == true;")
root = ast.ASTConditionNode(ast.ASTVariableNode("x123") , ast.ASTBinaryOpNode("=="), ast.ASTBoolNode("true"))
root.accept(print_visitor)
print("Node Count => ", print_visitor.node_count)

#code_generator = PArIRCodeGenerator()
#code = code_generator.generate_code(root)
#print(code)

code_generator_3 = PArIRCodeGenerator()
code_3 = code_generator_3.generate_code(root)
print("\nParIR Code:")
print(code_3)