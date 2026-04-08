# Examples

Complete, copy-pasteable examples of Sema Web applications. Each includes the `.sema` source file and the HTML host page.

## Counter

A minimal example demonstrating reactive state with `atom`, `put!`, and `update!`.

::: code-group

```scheme [counter.sema]
;; counter.sema — Reactive counter

(define count (atom 0))

(define (app)
  (sip (:div.counter
    (:h1 "Counter: " (->string (deref count)))
    (:div.buttons
      (:button {:on-click #(update! count dec)} "-")
      (:button {:on-click #(put! count 0)} "Reset")
      (:button {:on-click #(update! count inc)} "+")))))

(mount! (app) "#app")
```

```html [index.html]
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Sema Counter</title>
  <style>
    body { font-family: system-ui; display: flex; justify-content: center; padding: 4rem; }
    .counter { text-align: center; }
    .buttons { display: flex; gap: 0.5rem; justify-content: center; }
    button { padding: 0.5rem 1.5rem; font-size: 1.2rem; cursor: pointer; border-radius: 4px; border: 1px solid #ccc; }
  </style>
  <script type="text/sema" src="counter.sema"></script>
  <script type="module">
    import { SemaWeb } from "@sema-lang/sema-web";
    await SemaWeb.init();
  </script>
</head>
<body>
  <div id="app"></div>
</body>
</html>
```

:::

## Todo App

A more complete example showing lists, computed values, batched updates, multiple components, and scoped CSS.

::: code-group

```scheme [todo.sema]
;; todo.sema — Todo list with filters

(define todos (atom '()))
(define input-text (atom ""))
(define filter-mode (atom :all))  ; :all, :active, :done

(define next-id (atom 0))

(define (add-todo)
  (let ((text (deref input-text)))
    (when (not (string=? text ""))
      (batch
        (let ((id (deref next-id)))
          (update! next-id inc)
          (update! todos #(append % (list {:id id :text text :done false}))))
        (put! input-text "")))))

(define (toggle-todo id)
  (update! todos
    (lambda (ts)
      (map #(if (= (:id %) id)
               (assoc % :done (not (:done %)))
               %)
           ts))))

(define (remove-todo id)
  (update! todos #(filter (lambda (t) (not (= (:id t) id))) %)))

(define (visible-todos)
  (let ((mode (deref filter-mode))
        (ts (deref todos)))
    (cond
      ((eq? mode :active) (filter #(not (:done %)) ts))
      ((eq? mode :done)   (filter #(:done %) ts))
      (else ts))))

(define (remaining-count)
  (length (filter #(not (:done %)) (deref todos))))

(define (todo-input)
  (sip (:div.input-row
    (:input {:value (deref input-text)
             :placeholder "What needs to be done?"
             :on-input #(put! input-text (.-value (.-target %)))
             :on-keydown #(when (string=? (.-key %) "Enter") (add-todo))})
    (:button {:on-click add-todo} "Add"))))

(define (todo-item todo)
  (sip (:li {:class (if (:done todo) "done" "")}
    (:input {:type "checkbox"
             :checked (:done todo)
             :on-change #(toggle-todo (:id todo))})
    (:span.text (:text todo))
    (:button.remove {:on-click #(remove-todo (:id todo))} "x"))))

(define (todo-filters)
  (sip (:div.filters
    (:span (->string (remaining-count)) " items left")
    (:div.modes
      (for-each
        (lambda (mode)
          (sip (:button {:class (if (eq? (deref filter-mode) mode) "active" "")
                         :on-click #(put! filter-mode mode)}
            (keyword->string mode))))
        '(:all :active :done))))))

(define (app)
  (sip (:div.todo-app
    (:h1 "Todos")
    (todo-input)
    (:ul.todo-list
      (for-each todo-item (visible-todos)))
    (when (> (length (deref todos)) 0)
      (todo-filters)))))

(mount! (app) "#app")
```

```html [index.html]
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Sema Todos</title>
  <style>
    body { font-family: system-ui; display: flex; justify-content: center; padding: 2rem; background: #f5f5f5; }
    .todo-app { max-width: 480px; width: 100%; background: white; border-radius: 8px; padding: 1.5rem; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
    h1 { margin: 0 0 1rem; color: #333; }
    .input-row { display: flex; gap: 0.5rem; margin-bottom: 1rem; }
    .input-row input { flex: 1; padding: 0.5rem; border: 1px solid #ddd; border-radius: 4px; font-size: 1rem; }
    .input-row button { padding: 0.5rem 1rem; background: #4a9eff; color: white; border: none; border-radius: 4px; cursor: pointer; }
    .todo-list { list-style: none; padding: 0; margin: 0; }
    .todo-list li { display: flex; align-items: center; padding: 0.5rem 0; border-bottom: 1px solid #eee; gap: 0.5rem; }
    .todo-list li.done .text { text-decoration: line-through; color: #999; }
    .text { flex: 1; }
    .remove { background: none; border: none; color: #e55; cursor: pointer; font-size: 1rem; padding: 0 0.5rem; }
    .filters { display: flex; justify-content: space-between; align-items: center; margin-top: 1rem; font-size: 0.85rem; color: #888; }
    .modes { display: flex; gap: 0.25rem; }
    .modes button { background: none; border: 1px solid transparent; border-radius: 3px; cursor: pointer; padding: 2px 6px; font-size: 0.85rem; }
    .modes button.active { border-color: #4a9eff; color: #4a9eff; }
  </style>
  <script type="text/sema" src="todo.sema"></script>
  <script type="module">
    import { SemaWeb } from "@sema-lang/sema-web";
    await SemaWeb.init();
  </script>
</head>
<body>
  <div id="app"></div>
</body>
</html>
```

:::

## AI Chat

A streaming chat interface using `llm/chat-stream`. Requires a deployed [LLM proxy](./llm-proxy).

::: code-group

```scheme [chat.sema]
;; chat.sema — Streaming AI chat

(define messages (atom '()))
(define input-text (atom ""))
(define stream (atom nil))

(define (send-message)
  (let ((text (deref input-text)))
    (when (not (string=? text ""))
      ;; Add user message
      (update! messages #(append % (list {:role "user" :content text})))
      (put! input-text "")

      ;; Start streaming — build message list for the API
      (let ((msg-list (map #(message (string->keyword (:role %)) (:content %))
                           (deref messages))))
        (put! stream (llm/chat-stream msg-list {:model "gpt-4o"}))))))

(define (on-stream-done)
  ;; When stream completes, add assistant message to history
  (let ((s (deref stream)))
    (when (and s (:done (deref s)))
      (update! messages #(append % (list {:role "assistant"
                                          :content (:text (deref s))})))
      (put! stream nil))))

(define (message-bubble msg)
  (sip (:div {:class (string-append "message " (:role msg))}
    (:div.content (:content msg)))))

(define (stream-bubble)
  (let ((s (deref (deref stream))))
    (when (:text s)
      (sip (:div.message.assistant
        (:div.content (:text s))
        (unless (:done s)
          (sip (:span.cursor ">"))))))))

(define (app)
  (sip (:div.chat
    (:div.messages
      (for-each message-bubble (deref messages))
      (when (deref stream) (stream-bubble)))
    (:div.input-bar
      (:input {:value (deref input-text)
               :placeholder "Ask anything..."
               :on-input #(put! input-text (.-value (.-target %)))
               :on-keydown #(when (string=? (.-key %) "Enter")
                              (send-message))})
      (:button {:on-click send-message} "Send")))))

(mount! (app) "#app")
```

```html [index.html]
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Sema AI Chat</title>
  <style>
    * { box-sizing: border-box; margin: 0; }
    body { font-family: system-ui; background: #1a1a2e; color: #eee; height: 100vh; display: flex; justify-content: center; }
    .chat { max-width: 640px; width: 100%; height: 100vh; display: flex; flex-direction: column; }
    .messages { flex: 1; overflow-y: auto; padding: 1rem; display: flex; flex-direction: column; gap: 0.75rem; }
    .message { padding: 0.75rem 1rem; border-radius: 12px; max-width: 80%; line-height: 1.5; }
    .message.user { background: #4a9eff; align-self: flex-end; }
    .message.assistant { background: #2a2a4a; align-self: flex-start; }
    .cursor { animation: blink 0.7s step-end infinite; }
    @keyframes blink { 50% { opacity: 0; } }
    .input-bar { display: flex; gap: 0.5rem; padding: 1rem; border-top: 1px solid #333; }
    .input-bar input { flex: 1; padding: 0.75rem; border-radius: 8px; border: 1px solid #444; background: #2a2a4a; color: #eee; font-size: 1rem; }
    .input-bar button { padding: 0.75rem 1.5rem; border-radius: 8px; border: none; background: #4a9eff; color: white; cursor: pointer; font-size: 1rem; }
  </style>
  <script type="text/sema" src="chat.sema"></script>
  <script type="module">
    import { SemaWeb } from "@sema-lang/sema-web";
    await SemaWeb.create({
      llmProxy: "/api/llm",
    });
  </script>
</head>
<body>
  <div id="app"></div>
</body>
</html>
```

:::

This example requires a proxy endpoint at `/api/llm`. See the [Deployment](./deployment) guide for how to set one up with Vercel or Netlify.

The streaming works through the reactive system: `llm/chat-stream` returns a signal, and `(deref (deref stream))` in the component body creates a dependency. As tokens arrive and the signal updates, the component re-renders automatically.
