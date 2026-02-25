package com.sema.intellij

import com.intellij.lang.BracePair
import com.intellij.lang.PairedBraceMatcher
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IElementType

class SemaBraceMatcher : PairedBraceMatcher {
    override fun getPairs(): Array<BracePair> = arrayOf(
        BracePair(SemaTokenTypes.LPAREN, SemaTokenTypes.RPAREN, false),
        BracePair(SemaTokenTypes.LBRACKET, SemaTokenTypes.RBRACKET, false),
        BracePair(SemaTokenTypes.LBRACE, SemaTokenTypes.RBRACE, false),
    )

    override fun isPairedBracesAllowedBeforeType(lbraceType: IElementType, contextType: IElementType?): Boolean = true

    override fun getCodeConstructStart(file: PsiFile, openingBraceOffset: Int): Int = openingBraceOffset
}
