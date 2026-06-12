package com.sema.intellij

import com.sema.intellij.config.SemaBinary
import com.sema.intellij.config.SemaBinaryStatus
import org.junit.Assume

object TestHelpers {
    fun resolveSemaBinary(): SemaBinaryStatus {
        val explicitPath = System.getProperty("sema.test.binary")
        if (explicitPath != null) {
            return SemaBinary.check(explicitPath, emptyList())
        }
        val extraPath = System.getProperty("sema.test.path")
        val pathEntries = if (extraPath != null) {
            listOf(extraPath) + SemaBinary.pathEntriesFromEnvironment()
        } else {
            SemaBinary.pathEntriesFromEnvironment()
        }
        return SemaBinary.check("sema", pathEntries)
    }

    fun requireSema() {
        val status = resolveSemaBinary()
        Assume.assumeTrue(
            "Sema binary not found: ${status.errorText}. Set -Dsema.test.binary=/path/to/sema or ensure sema is on PATH.",
            status.available
        )
    }

    fun sampleSemaContent(): String = """
        ;;; Sample Sema file for testing
        (define greeting "Hello, world!")

        (defun square (x)
          "Return the square of X."
          (* x x))

        (defun factorial (n)
          (if (<= n 1)
              1
              (* n (factorial (- n 1)))))

        (when (> (square 5) 20)
          (print "5 squared exceeds 20"))

        (let ((a 10)
              (b 20))
          (+ a b))

        ;; Test various literals
        (define some-nil nil)
        (define some-bool #t)
        (define some-char #\a)
        (define some-keyword :name)
        (define some-quote '(1 2 3))
        (define some-list [1 2 3])
        (define some-map {:a 1 :b 2})

        ;; Import
        (import "math")
        (import "http")
    """.trimIndent()
}
