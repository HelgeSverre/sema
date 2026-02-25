package com.sema.intellij

import com.intellij.lang.ASTNode
import com.intellij.lang.ParserDefinition
import com.intellij.lang.PsiBuilder
import com.intellij.lang.PsiParser
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet

val SEMA_FILE = IFileElementType(SemaLanguage)

class SemaParser : PsiParser {
    override fun parse(root: IElementType, builder: PsiBuilder): ASTNode {
        val rootMarker = builder.mark()
        while (!builder.eof()) {
            builder.advanceLexer()
        }
        rootMarker.done(root)
        return builder.treeBuilt
    }
}

class SemaParserDefinition : ParserDefinition {
    override fun createLexer(project: Project?): Lexer = SemaLexer()
    override fun createParser(project: Project?): PsiParser = SemaParser()
    override fun getFileNodeType(): IFileElementType = SEMA_FILE
    override fun getWhitespaceTokens(): TokenSet = SemaTokenTypes.WHITESPACES
    override fun getCommentTokens(): TokenSet = SemaTokenTypes.COMMENTS
    override fun getStringLiteralElements(): TokenSet = SemaTokenTypes.STRINGS
    override fun createElement(node: ASTNode): PsiElement = LeafPsiElement(node.elementType, node.text)
    override fun createFile(viewProvider: FileViewProvider): PsiFile = SemaFile(viewProvider)
}
