#First some AST Node classes we'll use to build the AST with
class ASTNode:
    def __init__(self):
        self.name = "ASTNode"    

class ASTStatementNode(ASTNode):
    def __init__(self):
        self.name = "ASTStatementNode"

class ASTExpressionNode(ASTNode):
    def __init__(self):
        self.name = "ASTExpressionNode"

class ASTEndNode(ASTNode):
    def __init__(self):
        self.name = "ASTEndNode"
    
    def accept(self, visitor):
        visitor.visit_end_node(self)
    
class ASTReturnNode(ASTExpressionNode):
    def __init__(self, val):
        self.name = "ASTReturnNode"
        self.val = val
    
    def accept(self, visitor):
        visitor.visit_return_node(self)

class ASTVariableNode(ASTExpressionNode):
    def __init__(self, lexeme):
        self.name = "ASTVariableNode"
        self.lexeme = lexeme

    def accept(self, visitor):
        visitor.visit_variable_node(self)

class ASTDataTypeNode(ASTExpressionNode):
    def __init__(self, t):
        self.name = "ASTDataTypeNode"
        self.type = t
    
    def accept(self, visitor):
        visitor.visit_datatype_node(self)

class ASTIntegerNode(ASTExpressionNode):
    def __init__(self, v):
        self.name = "ASTIntegerNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_integer_node(self)        


class ASTBoolNode(ASTExpressionNode):
    def __init__(self, v):
        self.name = "ASTBoolNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_bool_node(self)  


class ASTFloatNode(ASTExpressionNode):
    def __init__(self, v):
        self.name = "ASTFloatNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_float_node(self)   


class ASTColourNode(ASTExpressionNode):
    def __init__(self, v):
        self.name = "ASTColourNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_colour_node(self)


class ASTWidthNode(ASTExpressionNode):
    def __init__(self, v):
        self.name = "ASTWidthNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_width_node(self)


class ASTHeightNode(ASTExpressionNode):
    def __init__(self, v):
        self.name = "ASTHeightNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_height_node(self)


class ASTArrayNode(ASTExpressionNode):
    def __init__(self, v, l):
        self.name = "ASTArrayNode"
        self.values = v
        self.length = l

    def accept(self, visitor):
        visitor.visit_array_node(self)


class ASTParenthesisNode:
    def __init__(self, expression):
        self.expression = expression

    def accept(self, visitor):
        visitor.visit_parenthesis_node(self)


class ASTOperatorNode():
    def __init__(self, op):
        self.name = "ASTOperatorNode"
        self.operator = op

    def accept(self, visitor):
        visitor.visit_operator_node(self) 


class ASTBinaryOpNode():
    def __init__(self, op):
        self.name = "ASTBinaryOpNode"
        self.binOp = op

    def accept(self, visitor):
        visitor.visit_binary_operator_node(self)


class ASTModExpressionNode(ASTExpressionNode):
    def __init__(self, lhs, op, rhs):
        self.name = "ASTModExpressionNode"
        self.id = lhs
        self.operator = op
        self.expr = rhs
    
    def accept(self, visitor):
        visitor.visit_mod_expr_node(self)

class ASTAssignmentNode(ASTStatementNode):
    def __init__(self, ast_var_node, ast_expression_node):
        self.name = "ASTAssignmentNode"        
        self.id   = ast_var_node
        self.expr = ast_expression_node

    def accept(self, visitor):
        visitor.visit_assignment_node(self)

class ASTAssignmentArrayNode(ASTStatementNode):
    def __init__(self, ast_var_node, ast_array_node):
        self.name = "ASTAssignmentArrayNode"        
        self.id   = ast_var_node
        self.expr = ast_array_node

    def accept(self, visitor):
        visitor.visit_assignment_array_node(self)


class ASTArrayElementNode(ASTExpressionNode):
    def __init__(self, index, ast_var_node):
        self.name = "ASTNewArrayElementNode"
        self.index = index
        self.arr_node = ast_var_node
    
    def accept(self, visitor):
        visitor.visit_array_element_node(self)

class ASTVarDeclNode(ASTStatementNode):
    def __init__(self, ast_var_node, ast_type_node, ast_expression_node):
        self.name = "ASTVarDeclNode"        
        self.id   = ast_var_node
        self.type = ast_type_node
        self.expr = ast_expression_node

    def accept(self, visitor):
        visitor.visit_var_decl_node(self)


class ASTVarDeclArrayNode(ASTStatementNode):
    def __init__(self, ast_var_node, ast_type_node, ast_array_node):
        self.name = "ASTVarDeclArrayNode"        
        self.id   = ast_var_node
        self.type = ast_type_node
        self.expr = ast_array_node

    def accept(self, visitor):
        visitor.visit_var_decl_array_node(self)


class ASTConditionNode():
    def __init__(self, ast_var_node, ast_relOp_node, ast_expression_node):
        self.name = "ASTConditionNode"        
        self.id   = ast_var_node
        self.relOp = ast_relOp_node
        self.expr = ast_expression_node
        self.cond_full = None
        if (type(self.expr) == ASTVariableNode):
            if (type(self.id) == ASTVariableNode):
                self.cond_full = str(self.id.lexeme) + ' ' + str(self.relOp.binOp) + ' ' + str(self.expr.lexeme)
            else:
                self.cond_full = str(self.id.value) + ' ' + str(self.relOp.binOp) + ' ' + str(self.expr.lexeme)
        else:
            if (type(self.id) == ASTVariableNode):
                self.cond_full = str(self.id.lexeme) + ' ' + str(self.relOp.binOp) + ' ' + str(self.expr.value)
            else:
                self.cond_full = str(self.id.value) + ' ' + str(self.relOp.binOp) + ' ' + str(self.expr.value)

    def accept(self, visitor):
        visitor.visit_condition_node(self)


class ASTBlockNode(ASTNode):
    def __init__(self):
        self.name = "ASTBlockNode"
        self.stmts = []

    def add_statement(self, node):
        self.stmts.append(node)

    def accept(self, visitor):
        visitor.visit_block_node(self)  


class ASTProgramNode(ASTNode):
    def __init__(self):
        self.name = "ASTProgramNode"
        self.stmts = []

    def add_statement(self, node):
        self.stmts.append(node)

    def accept(self, visitor):
        visitor.visit_program_node(self) 


class ASTIfNode:
    def __init__(self, condition, if_body):
        self.name = "ASTIfNode"
        self.condition = condition
        self.if_body = if_body

    def accept(self, visitor):
        visitor.visit_if_node(self)


class ASTIfElseNode:
    def __init__(self, condition, if_body, else_body):
        self.name = "ASTIfElseNode"
        self.condition = condition
        self.if_body = if_body
        self.else_body = else_body

    def accept(self, visitor):
        visitor.visit_if_else_node(self)


class ASTForNode:
    def __init__(self, initalisation, condition, incrementation, for_body):
        self.name = "ASTForNode"
        self.var_init = initalisation
        self.condition = condition
        self.var_inc = incrementation
        self.for_body = for_body

    def accept(self, visitor):
        visitor.visit_for_node(self)


class ASTWhileNode:
    def __init__(self, condition, while_body):
        self.name = "ASTForNode"
        self.condition = condition
        self.while_body = while_body

    def accept(self, visitor):
        visitor.visit_while_node(self)
    

class ASTFunctionDeclNode:
    def __init__(self, ast_name, ast_type) -> None:
        self.name = "ASTFunctionDeclNode"
        self.func_name = ast_name
        self.func_type = ast_type
    
    def accept(self, visitor):
        visitor.visit_function_decl_node(self)


class ASTFunctionDeclNodeWithParams(ASTFunctionDeclNode):
    def __init__(self, ast_name, ast_type, params) -> None:
        self.name = "ASTFunctionDeclNodeWithParams"
        self.func_name = ast_name
        self.func_type = ast_type
        self.params = params
    
    def accept(self, visitor):
        visitor.visit_function_decl_with_params_node(self)


class ASTFunctionNode:
    def __init__(self, func_decl, func_body) -> None:
        self.name = "ASTFunctionNode"
        self.func_decl = func_decl
        self.func_body = func_body
    
    def accept(self, visitor):
        visitor.visit_function_node(self)


class ASTFunctionCallNode:
    def __init__(self, func_name) -> None:
        self.name = "ASTFunctionCallNode"
        self.func_name = func_name
    
    def accept(self, visitor):
        visitor.visit_function_call_node(self)


class ASTPrintNode:
    def __init__(self, lexeme):
        self.name = "ASTPrintNode"
        self.expr = lexeme
    
    def accept(self, visitor):
        visitor.visit_print_node(self)

class ASTDelayNode:
    def __init__(self, lexeme):
        self.name = "ASTDelayNode"
        self.expr = lexeme
    
    def accept(self, visitor):
        visitor.visit_delay_node(self)

class ASTClearNode:
    def __init__(self, lexeme):
        self.name = "ASTClearNode"
        self.expr = lexeme
    
    def accept(self, visitor):
        visitor.visit_clear_node(self)

class ASTWriteNode():
    def __init__(self, colour, y, x) -> None:
        self.name = "ASTWriteNode"
        self.colour = colour
        self.yPos = y
        self.xPos = x
    
    def accept(self, visitor):
        visitor.visit_write_node(self)

class ASTWriteBoxNode():
    def __init__(self, colour, y, x, h, w) -> None:
        self.name = "ASTWriteNode"
        self.colour = colour
        self.yPos = y
        self.xPos = x
        self.height = h
        self.width = w
    
    def accept(self, visitor):
        visitor.visit_write_box_node(self)

class ASTRandomNode:
    def __init__(self, lexeme):
        self.name = "ASTRandomNode"
        self.expr = lexeme
    
    def accept(self, visitor):
        visitor.visit_random_node(self)