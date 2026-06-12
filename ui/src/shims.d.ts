// Vite resolves `?inline` CSS imports to the stylesheet text as a string.
declare module '*.css?inline' {
  const css: string;
  export default css;
}
