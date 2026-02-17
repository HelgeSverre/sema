import { test, expect } from '@playwright/test';

test('WASM memory growth tracking', async ({ page }) => {
  await page.goto('/');
  await page.waitForSelector('[data-testid="status"].status-ready', { timeout: 15000 });

  // Define the noise functions
  const defs = `
(define seed 42)
(define (hash x y)
  (let* ((h (mod (+ (* (abs x) 374761393) (* (abs y) 668265263)
                    (* seed 1274126177)) 1000000003))
         (h2 (mod (* h h) 1000000007)))
    (/ (mod (abs h2) 1000000) 1000000.0)))
(define (fade t)
  (let ((t3 (* t t t)))
    (* t3 (+ (* t (- (* t 6.0) 15.0)) 10.0))))
(define (lerp a b t) (+ a (* (- b a) t)))
(define (value-noise x y)
  (let* ((ix (floor x)) (iy (floor y))
         (fx (- x ix)) (fy (- y iy))
         (u (fade fx)) (v (fade fy)))
    (lerp (lerp (hash ix iy) (hash (+ ix 1) iy) u)
          (lerp (hash ix (+ iy 1)) (hash (+ ix 1) (+ iy 1)) u)
          v)))
(define (octave-noise x y)
  (let loop ((i 0) (freq 1.0) (amp 1.0) (total 0.0) (max-amp 0.0))
    (if (= i 3) (/ total max-amp)
      (loop (+ i 1) (* freq 2.0) (* amp 0.5)
            (+ total (* amp (value-noise (* x freq 0.08) (* y freq 0.08))))
            (+ max-amp amp)))))
`;

  await page.getByTestId('editor').fill(defs + '\n"defs loaded"');
  await page.getByTestId('run-btn').click();
  await page.waitForSelector('#output .output-timing', { timeout: 10000 });

  // Check memory usage via performance.measureUserAgentSpecificMemory or performance.memory
  for (let i = 0; i < 10; i++) {
    const memInfo = await page.evaluate(() => {
      // @ts-ignore
      const perf = performance as any;
      return {
        jsHeapUsed: perf.memory?.usedJSHeapSize,
        jsHeapTotal: perf.memory?.totalJSHeapSize,
      };
    });
    
    await page.getByTestId('editor').fill(`(octave-noise ${i} 0)`);
    try {
      await page.getByTestId('run-btn').click();
      await page.waitForSelector('#output .output-timing', { timeout: 5000 });
      console.log(`Call ${i}: OK, heap=${memInfo.jsHeapUsed ? Math.round(memInfo.jsHeapUsed / 1024 / 1024) + 'MB' : 'N/A'}`);
    } catch (e: any) {
      console.log(`Call ${i}: CRASHED, heap was ${memInfo.jsHeapUsed ? Math.round(memInfo.jsHeapUsed / 1024 / 1024) + 'MB' : 'N/A'}`);
      return;
    }
  }
});

test('direct JS eval to avoid page crash', async ({ page }) => {
  page.on('console', msg => { if (msg.type() === 'error') console.log(`[err] ${msg.text()}`); });
  
  await page.goto('/');
  await page.waitForSelector('[data-testid="status"].status-ready', { timeout: 15000 });

  // Run the full perlin noise via page.evaluate to catch the WASM error
  const result = await page.evaluate(async () => {
    // Get the WASM module
    const mod = await import('/pkg/sema_wasm.js');
    await mod.default();
    const interp = new mod.WasmInterpreter();
    
    try {
      const code = `
(define seed 42)
(define (hash x y)
  (let* ((h (mod (+ (* (abs x) 374761393) (* (abs y) 668265263)
                    (* seed 1274126177)) 1000000003))
         (h2 (mod (* h h) 1000000007)))
    (/ (mod (abs h2) 1000000) 1000000.0)))
(define (fade t) (let ((t3 (* t t t))) (* t3 (+ (* t (- (* t 6.0) 15.0)) 10.0))))
(define (lerp a b t) (+ a (* (- b a) t)))
(define (value-noise x y)
  (let* ((ix (floor x)) (iy (floor y)) (fx (- x ix)) (fy (- y iy)) (u (fade fx)) (v (fade fy)))
    (lerp (lerp (hash ix iy) (hash (+ ix 1) iy) u) (lerp (hash ix (+ iy 1)) (hash (+ ix 1) (+ iy 1)) u) v)))
(define (octave-noise x y)
  (let loop ((i 0) (freq 1.0) (amp 1.0) (total 0.0) (max-amp 0.0))
    (if (= i 3) (/ total max-amp)
      (loop (+ i 1) (* freq 2.0) (* amp 0.5) (+ total (* amp (value-noise (* x freq 0.08) (* y freq 0.08)))) (+ max-amp amp)))))
(define (terrain-char val)
  (cond ((< val 0.30) "~") ((< val 0.38) ".") ((< val 0.50) ",") ((< val 0.60) ";") ((< val 0.72) ":") ((< val 0.85) "%") (#t "#")))
(do ((y 0 (+ y 1))) ((= y 20))
  (do ((x 0 (+ x 1))) ((= x 60))
    (display (terrain-char (octave-noise x y))))
  (newline))
`;
      return interp.eval_global(code);
    } catch (e: any) {
      return `JS ERROR: ${e.message} / ${e.stack?.substring(0, 200)}`;
    }
  });
  
  console.log('Result:', typeof result === 'string' ? result.substring(0, 200) : result);
});
