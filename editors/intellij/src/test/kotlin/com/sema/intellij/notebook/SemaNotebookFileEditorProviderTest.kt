package com.sema.intellij.notebook

import com.intellij.openapi.fileEditor.FileEditorPolicy
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.testFramework.LightPlatformTestCase
import com.sema.intellij.SemaFileType
import org.junit.Assert.*
import org.junit.Test

class SemaNotebookFileEditorProviderTest : LightPlatformTestCase() {

    private fun virtualFile(name: String, ft: com.intellij.openapi.fileTypes.FileType): VirtualFile =
        object : VirtualFile() {
            override fun getName() = name
            override fun getFileSystem() = throw UnsupportedOperationException()
            override fun getPath() = name
            override fun isDirectory() = false
            override fun isValid() = true
            override fun getParent(): VirtualFile? = null
            override fun getChildren(): Array<VirtualFile> = emptyArray()
            override fun getOutputStream(o: Any, n1: Long, n2: Long) = throw UnsupportedOperationException()
            override fun contentsToByteArray() = ByteArray(0)
            override fun getTimeStamp() = 0L
            override fun getLength() = 0L
            override fun refresh(a: Boolean, b: Boolean, r: Runnable?) {}
            override fun getInputStream() = throw UnsupportedOperationException()
            override fun isWritable() = false
            override fun getModificationStamp() = 0L
            override fun getFileType() = ft
        }

    @Test
    fun testAcceptsSemaNbFiles() {
        val provider = SemaNotebookFileEditorProvider()
        assertTrue(provider.accept(project, virtualFile("test.sema-nb", SemaNotebookFileType)))
    }

    @Test
    fun testRejectsNonSemaNbFiles() {
        val provider = SemaNotebookFileEditorProvider()
        assertFalse(provider.accept(project, virtualFile("test.sema", SemaFileType)))
    }

    @Test
    fun testEditorTypeId() = assertEquals("sema-notebook-editor", SemaNotebookFileEditorProvider().editorTypeId)

    @Test
    fun testPolicy() = assertEquals(FileEditorPolicy.HIDE_DEFAULT_EDITOR, SemaNotebookFileEditorProvider().policy)
}
