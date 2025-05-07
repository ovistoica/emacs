This file is a merged representation of a subset of the codebase, containing specifically included files, combined into a single document by Repomix.
The content has been processed where empty lines have been removed, content has been compressed (code blocks are separated by ⋮---- delimiter), security check has been disabled.

# Directory Structure
```
docs/
  a-loop.md
  api-builtin-effects.md
  api-intro.md
  App-Structure.md
  Coeffects.md
  dominoes-30k.md
  Effects.md
  flow-mechanics.md
  flows-advanced-topics.md
  Flows.md
  Interceptors.md
  Loading-Initial-Data.md
  re-frame.md
  reusable-components.md
  start-coding.md
  subscriptions.md
  Talking-To-Servers.md
  Testing.md
examples/
  flow/
    src/
      re_frame/
        flow/
          demo.cljs
  simple/
    src/
      simple/
        core.cljs
  todomvc/
    src/
      todomvc/
        core.cljs
        db.cljs
        events.cljs
        subs.cljs
        views.cljs
src/
  re_frame/
    flow/
      alpha.cljc
    query/
      alpha.cljc
    subs/
      alpha.cljc
    cofx.cljc
    core.cljc
    db.cljc
    events.cljc
    fx.cljc
    interceptor.cljc
    subs.cljc
README.md
```

# Files

## File: docs/a-loop.md
````markdown
## Homoiconic

ClojureScript is a modern LISP, and LISPs are **homoiconic**.

You program in a LISP by creating and assembling LISP data structures. The syntax is data literals.
Dwell on that for a moment. You are **_programming in data_**.
The functions which later transform data, themselves start as data.
Computation involves evaluating data. Macros, running at compile time, take code (which is just data)
and rewrite it to other code (other data). The duality of code and data runs deep.

<img align="right" width="220" src="../images/yinyang.png">

So, Clojurists place particular emphasis on the primacy of data.

They meditate on aphorisms like **_data is the ultimate in late binding_**. They
exalt inequalities like `data > functions > macros`. (They also
re-watch Rich Hickey videos a bit too much, and wish that
their hair was darker and more curly.)


I cannot stress enough what a big deal this is. It will seem
like a syntax curiosity at first but, when the penny drops for
you on this, it tends to be a profound moment.

So, it will come as no surprise, then, to find that re-frame has a
data-oriented design. Events are data. Effects are data. DOM is data.
The functions which transform data are registered and looked up via
data. Interceptors (data) are preferred to middleware (higher
order functions). Etc.

And re-frame apps are reactive
which further elevates data because in reactive systems,
it is the arrival of data which [coordinates the calling of functions](https://www.youtube.com/watch?v=ZgqFlowyfTA&t=80), not the other way around.

Data - that's the way we roll.

## The Data Loop

Architecturally, re-frame implements "a perpetual loop".

To build an app, you hang pure functions on certain parts of this loop,
and re-frame looks after the **conveyance of data**
around the loop, into and out of the transforming functions you
provide. The tag line for re-frame is "derived values, flowing".

Remember this diagram from school? The water cycle, right?

<img height="290px" align="right" src="../images/the-water-cycle.png?raw=true">

Two distinct stages, involving water in different phases, being acted upon
by different forces: gravity working one way, evaporation and convection the other.

To understand re-frame, **imagine data flowing around that loop instead of water**.

re-frame provides the conveyance of the data around the loop - the equivalent
of gravity, evaporation and convection.
You design what's flowing, and then you hang functions on the loop at
various points to compute the data's phase changes.

Sure, right now, you're thinking "lazy sod - make a proper Computer Science-y diagram". But, no.
Joe Armstrong says "don't break the laws of physics" - I'm sure
you've seen the videos - and if he says to do something, you do it
(unless Rich Hickey disagrees, and says to do something else).

So, this diagram, apart from being a plausible analogy which might help
you to understand re-frame, is **practically proof** it does physics.

<!--  too many jokes for one page

## On Bourgeois Claims

It isn't easy being a framework creator. We're constantly buffeted by the impressive marketing claims of
new entrants. These shiny new things are at once "blazingly fast" and "elegantly" reducing something bad,
while also being utterly awesome at ... good things. It is easy to feel insecure about your baby.

Of course, a few months later, you read frustrated tweets calling out the design tradeoffs of these new wonders.

For a brief period there, I engaged. I insisted that "re-frame puts state into state of the art"
and I felt pretty smug about that.  Don't judge me - I said it was difficult.

But I'm older and wiser now. These days I only claim only that re-frame involves
"derived data flowing" and that it provides state management via six dominoes.
-->

## Six Dominoes

Each iteration of the re-frame loop has 6 stages, and because these stages happen one after the other,
we talk about this process as a six domino cascade.

One domino triggers the next, which triggers the next, boom, boom, boom, until we are
back at the beginning of the loop, and the dominoes reset to attention
again, ready for the next iteration of the same cascade.

<img align="right" src="../images/Readme/Dominoes-small.jpg">

The six dominoes are:

1. Event dispatch
2. Event handling
3. Effect handling
4. Query
5. View
6. DOM

Let's begin by looking at each of them from a great height - maybe **from 60,000 feet**.

## 1st Domino - Event Dispatch

An **_event_** is sent when something happens - the user
clicks a button, or a websocket receives a new message.

Without the impulse of a triggering `event`, no six domino cascade occurs.
It is only because of `events` that a re-frame app is propelled,
loop iteration after loop iteration, from one state to the next.

**re-frame is event driven.**

## 2nd Domino - Event Handling

In response to an `event`, an application must decide what action to take.
This is known as **event handling**.

Event handler functions compute how an event should change "the world",
which is to say that they compute the `side effects` of the event.
Or, more accurately, they compute **a declarative description** of the required
`side effects` - represented as data.

So `event handlers` are just functions which compute data, and that data describes what needs to happen.

Much of the time, an event will only cause `side effects` to
"application state", but sometimes the outside world must also be affected:
localstore, cookies, databases, emails, logs, etc.

## 3rd Domino - Effect Handling

In this step, the `side effects`, calculated by the previous step, are actioned.

Data gets turned into action and the world is mutated.


<img align="left" width="305" src="../images/Alien3_0.jpg">


Now, to a functional programmer, `effects` are scary in a
xenomorph kind of way. Nothing messes with functional purity
quite like the need for side effects.

On the other hand, `effects` are
marvelous because they move the app forward. Without them,
an app stays stuck in one state forever, never achieving anything.

So re-frame embraces the protagonist nature of `effects` - the entire, unruly zoo of them - but
it does so in a controlled and largely hidden way, and in a manner which is debuggable, auditable, mockable and pluggable.


## We're Now At A Pivot Point

Domino 3 just changed the world and, very often, one particular part of it: the **application state**.

re-frame's `application state` is held in one place - think of it like you
would an in-memory, central database for the app (details soon).

Any changes to `application state` trigger the next part of the cascade
involving dominoes 4-5-6.

## There's a Formula For It

The 4-5-6 domino cascade implements the formula made famous by Facebook's ground-breaking React library: `v = f(s)`

A view, `v`, is a function, `f`, of the app state, `s`.

Said another way, there are functions `f` that compute which DOM nodes, `v`,
should be displayed to the user when the application is in a given app state, `s`.

Or, to capture the dynamics we'd say: **over time**, as `s` changes, `f`
will be re-run each time to compute new `v`, forever keeping `v` up to date with the current `s`.

Or, with yet another emphasis: **over time** what is presented to the user changes in response to application state changes.

In our case, domino 3 changes `s`, the application state,
and, in response, dominoes 4-5-6 are concerned with re-running `f` to compute the new `v`
shown to the user.

Except, there's no single `f` to run. There are many `f` which
collectively build the overall `v`. And only a certain part of `s`
may have changed, meaning only a subset of
the `f` need rerun, to re-compute a subset of `v`.


## Domino 4 - Query

<img align="right" src="../images/Readme/6dominoes.png?raw=true">

Domino 4 is about extracting data from "app state", and providing it
in the right format for the `ViewFunctions` of domino 5.

Domino 4 is a novel and efficient de-duplicated `Signal Graph` which
runs query functions on the app state, efficiently computing
reactive, multi-layered, "materialised views" of it.

Please relax about any unfamiliar terminology, you'll soon
see how simple the code is in practice.


## Domino 5 - View

Domino 5 is many **ViewFunctions** (aka Reagent components) which collectively
render the UI of the application.

Each `ViewFunction` renders part of the whole. These functions compute and return
data in a format called **hiccup** which represents DOM.

To render the right DOM, `ViewFunctions` must obtain state using the signal graph of domino 4.
They use a `subscribe` facility which reactively delivers this state. They automatically re-run
in response to changes in the Signal Graph, keeping the UI up to date.

So, after the application state changes in domino 3, data flows through the Signal Graph of domino 4, causing
the `ViewFunctions` to re-render the UI presented to the user.


## Domino 6 - DOM

You don't write Domino 6 - it is handled for you
by Reagent/React. I mention it here
for completeness and to fully close the loop.

This is the step in which the hiccup-formatted
"descriptions of required DOM", returned by the `ViewFunctions` of Domino 5, are actioned.
The browser DOM nodes are mutated.
````

## File: docs/api-builtin-effects.md
````markdown
# Builtin Effects

re-frame supplies a small number of built-in effects which contribute to the API.

## What Are Effects?

Event handlers, such as those registered using `reg-event-fx`, compute and return a map of effects which might look like this:
```clj
{:db  new-db
 :fx  [ [:dispatch [:some-id]]
        [:full-screen true]
        [:http     {:method :GET  :url "http://somewhere.com/"}]]}
```
That's a map with two keys:  `:db` and `:fx`.  Which means there are two effects in this example. In another case, there could be
more or less.

Each effect consists of an `id` and a `payload` pair. The `id` identifies the effect required and the `payload`
carries additional information which will parameterise the action taken.

So, if an effect map was `#!clj {:db new-value}`, it would contain only one effect with an `id` of `:db` and a payload of `new-value`.

In the example above, the effect `:fx` has a vector payload.
That vector is a sequence of other effects, each captured as a 2-vector: `[id payload]` - for example `[:full-screen true]`.
`:fx` is an effect which actions other effects in sequence - the ones in its payload.

The two effects `:db` and `:fx` are a powerful combination, and both of these effects are built-in. That means re-frame itself
uses `reg-fx` to register effect handlers for these two `ids`. Other effects, like `:http`, might come from a third-party library
or from within your own application code.

This page lists the built-in effects.

## <a name="db"></a> :db

`reset!` `app-db` to be a new value. The associated `value` is expected to be a map.

The `:db` effect has a special status. It will always be actioned before others. (Prior to v1.1.0 this guarantee did not exist. There were no ordering guarantees).

usage:
```clojure
{:db  some-map}
```

In the wild, real usage might look like this:
```clojure
(reg-event-fx
  :token
  (fn [{:keys [db]} event]
    {:db  (assoc db :some-key some-val)}))     ;; <-- new value computed
```

## <a name="fx"></a> :fx

> Added in v1.1.0

An effect which actions other effects, sequentially.

Expects a value which is a sequence, typically a vector.
Each element in the sequence represents one effect.
Each element is a 2-tuple of (1) an effect id and (2) the payload of the effect (the value ultimately given to the registered effect handler as an argument).

For example:
```clj
{:db  new-db
 :fx  [ [:dispatch   [:some-id "extra"]]
        [:http-xhrio {:method :GET  :url "http://somewhere.com/"}]
        (when (> 2 3) [:full-screen true])]}
```

Notice the use of `when` to conditionally include or exclude an effect. Any `nil` found in a `:fx` sequence will be ignored.

## <a name="dispatch"></a> :dispatch

`dispatch` one event. Expects a single vector.

usage:
```clojure
{:fx [[:dispatch [:event-id "param1" :param2]]] }
```

To dispatch multiple events:
```clojure
{:fx [[:dispatch [:event1 "param1" :param2]]
      [:dispatch [:second]]}
```
Effects in `:fx` are actioned in order, so the dispatched events will be queued and, later handled, in order supplied. FIFO.

## <a name="dispatch-later"></a> :dispatch-later

`dispatch` one or more events after a given delay. Expects a payload which is a
map with two keys: `:ms` (milliseconds delay), and `:dispatch` (the event).

usage:
```clojure
{:fx [ [:dispatch-later {:ms 200 :dispatch [:event-id1 "param"]}]
       [:dispatch-later {:ms 100 :dispatch [:event-id2 "param"]}]]}
```

Prior to re-frame v1.1.1 `:dispatch-later` required a seq of maps, since v1.1.1 it
can also accept a single map.

## <a name="deregister-event-handler"></a> :deregister-event-handler

Removes a previously registered event handler. Expects the event id for a previously registered event handler.

usage:
```clojure
{:db new-db
 :fx [[:deregister-event-handler :my-id]])}
```


## <a name="dispatch-n"></a> :dispatch-n

> From v1.1.0 onwards, this effect is **deprecated** in favour of using `:fx` with multiple `:dispatch` tuples.

`dispatch` more than one event. Expects a seq of event vectors (typically a list of them).

usage:
```clojure
{:db new-db
 :fx [[:dispatch-n (list [:do :all] [:three :of] [:these])]]}
```
Notes:

  1. The events will be dispatched in the order provided. And, because events are handled FIFO, the events will subsequently be processed in the order provided.
  2. nils in the event collection are ignored which means events can be added
conditionally:

     ```clojure
     {:db new-db
      :fx [[:dispatch-n (list (when (> 3 5) [:conditioned-out])
                           [:another-one])]]}
     ```
````

## File: docs/api-intro.md
````markdown
# Overview

The re-frame API consists of:

  - the namespace `re-frame.core`
  - a set of built-in effects

In the navigation to the left, you'll see a link to both.

## Dependency Information

Please review both the [releases page](http://day8.github.io/re-frame/releases/2022) and the [Clojars page](https://clojars.org/re-frame/) to discover the version you should be using.


## Using re-frame

To use the re-frame API within your namespace, you'll
need to `require` it, perhaps like this:
```clj
(ns my-app.some-namespace
  (:require [re-frame.core :as rf]))

;; your code here
```

You'll then be able to use the functions in the API, perhaps like this: `#!clj rf/dispatch`.


## The Most Commonly Used Part Of The API

When you are writing `View Functions`:

  - `dispatch` (or occasionally, `dispatch-sync`)
  - `subscribe`

When you are registering:

  - event handlers - `reg-event-db` and `reg-event-fx`
  - subscription handlers - `reg-sub` (and rarely `reg-sub-raw`)
  - rarely, effect handlers - `reg-fx`
  - rarely, coeffect handlers - `reg-cofx` with `inject-cofx`

When you register `event handlers`, you might use builtin interceptors:

  - `path`
  - `on-change`
  - `enrich`
  - `after`
  - `trim-v`
  - `debug`

Global interceptors can be very useful:

  - register them via `reg-global-interceptors`
  - rarely, remove them via `clear-global-interceptor`

When errors arise:

  - Catch them from events and interceptors via `reg-event-error-handler`

## More Rarely Used Part

Testing or dev-time related utilities:

  - `clear-subscription-cache!`
  - `make-restore-fn`
  - `purge-event-queue`

Logging/debugging:

  - `console`
  - `set-loggers`


If you write an Interceptor, use these utilities. To see how they are used, look
at the [re-frame code for builtin Interceptors](https://github.com/day8/re-frame/blob/master/src/re_frame/std_interceptors.cljc):

  - `->interceptor`
  - `get-coeffect`
  - `assoc-coeffect`
  - `get-effect`
  - `assoc-effect`
  - `enqueue`
````

## File: docs/App-Structure.md
````markdown
## A Smaller App

For simpler apps, you should put code for each layer into separate files:
<pre>
src
├── core.cljs         <--- entry point, plus history, routing, etc
├── db.cljs           <--- schema, validation, etc  (data layer)
├── views.cljs        <--- reagent views (view layer)
├── events.cljs       <--- event handlers (control/update layer)
└── subs.cljs         <--- subscription handlers  (query layer)
</pre>

For a living example of this approach, look at the [todomvc example](https://github.com/day8/re-frame/tree/master/examples/todomvc).

## The Gotcha

If you adopt this structure, there's a gotcha.

`events.cljs` and `subs.cljs` will never be `required` by any other
namespaces. To the Google Closure dependency mechanism, it appears as
if these two namespaces are not needed and it doesn't load them.

And, if the namespaces are not loaded, the registrations in these namespaces will
never happen. And, then you'll be staring at your running app very
puzzled about why none of your events handlers are registered.

Once you twig to what's going on, the solution is easy.  You must
explicitly `require` both namespaces, `events` and `subs`, in your `core`
namespace. Then they'll be loaded and the registrations (`reg-sub`, `reg-event-fx`,
etc) will occur as that loading happens.

## Larger Apps

Assuming your larger apps have multiple "panels" (or "views") which are
relatively independent, you might use this structure:
<pre>
src
├── core.cljs             <--- entry point, plus history, routing, etc
├── panel-1
│   ├── db.cljs           <--- schema, validation, etc  (data layer)
│   ├── subs.cljs         <--- subscription handlers  (query layer)
│   ├── views.cljs        <--- reagent components (view layer)
│   └── events.cljs       <--- event handlers (control/update layer)
├── panel-2
│   ├── db.cljs           <--- schema, validation. etc  (data layer)
│   ├── subs.cljs         <--- subscription handlers  (query layer)
│   ├── views.cljs        <--- reagent components (view layer)
│   └── events.cljs       <--- event handlers (control/update layer)
.
.
└── panel-n
</pre>


## Namespaced Ids

As an app gets bigger, you'll tend to get clashes on ids - event-ids, or query-ids (subscriptions), etc.

One panel will need to `dispatch` an `:edit` event and so will
another, but the two panels will have different handlers.
So, how do you avoid a clash? How do you distinguish between
one `:edit` event and another?

Your goal should be to use event-ids which encode both the event
itself (`:edit` ?) and the context (`:panel1` or `:panel2` ?).

Luckily, ClojureScript provides a nice easy solution: use keywords
with a __synthetic namespace__. Perhaps something like `:panel1/edit` and `:panel2/edit`.

You see, ClojureScript allows the namespace in a keyword to be a total
fiction. I can have the keyword `:panel1/edit` even though
`panel1.cljs` doesn't exist.

Naturally, you'll take advantage of this by using keyword namespaces
which are both unique and descriptive.

## Navigation


How do I switch between different panels of a larger app?

Your `app-db` could have an `:active-panel` key containing an id for the panel being displayed.


When the user does something navigation-ish (selects a tab, a dropdown or something which changes the active panel), then the associated event and dispatch look like this:

```clj
(re-frame/reg-event-db
  :set-active-panel
  (fn [db [_ value]]
    (assoc db :active-panel value)))

(re-frame/dispatch
  [:set-active-panel :panel1])
```

A high level reagent view has a subscription to :active-panel and will switch to the associated panel.

```clj
(re-frame/reg-sub
  :active-panel
  (fn [db _]
    (:active-panel db)))

(defn panel1
 []
 [:div  {:on-click #(re-frame/dispatch [:set-active-panel :panel2])}
        "Here" ])

(defn panel2
 []
 [:div "There"])

(defn high-level-view
  []
  (let [active  (re-frame/subscribe [:active-panel])]
    (fn []
      [:div
       [:div.title   "Heading"]
       (condp = @active                ;; or you could look up in a map
         :panel1   [panel1]
         :panel2   [panel2])])))
```
````

## File: docs/Coeffects.md
````markdown
# Coeffects

This tutorial explains `coeffects`.

It explains what they are, how they can be "injected", and how
to manage them in tests.

## What Are They?

Event handlers compute how the world should change in response to an event and, to do that,
they need to first know the current state of the world.

`coeffects` is the current state of the world, as data, as presented to an event handler.

Many event handlers only need application state to do their job - that's as much of "the world"
as they need to know about. To make this common case easy to program,
there's a specific registration function, called `reg-event-db`,
which delivers ONLY the coeffect `db` to the event handler  (and `event` of course).

Such an event handler will have this signature:
```clj
(fn [db event]
   ... return updated db)
```

But event handlers sometimes need to know more about the world OR have more inputs
than just application state. Sometimes they need "inputs" like a random number, or a GUID,
or the current datetime. Perhaps they need access to LocalStore, or Cookies, or a
DataScript connection.

We refer to these inputs collectively as the event handler's `coeffects`.  When more than
application state is needed, we use the registration function `reg-event-fx` and the event handler has
a signature like this:
```clj
(fn [coeffects event]     ;; first arg is often abbreviated to cofx
    ... return a map of effects)
```

Notice how previously the first arg was `db` and now it is `coeffects`.  `coeffects` is a map, and it contains a
`:db` key which is the current application state. But it can contain other keys holding data about other aspects of
the world. So `coeffects` is a superset of `db`. It is a bigger world to compute against.

##  A Motivating Example

Imagine you had an event handler which needed to "know" a value in LocalStore, in order to
compute an event's effect.

It could be written to access data directly from LocalStore:
```clj
(reg-event-db
   :load-defaults
   (fn [db _]
     (let [val (js->clj (.getItem js/localStorage "defaults-key"))]  ;; <-- Problem
       (assoc db :defaults val))))
```

This works, but there's a cost.

Because it has directly accessed LocalStore, this event handler is not
pure, and impure functions cause well-documented paper cuts, and paper cuts
have a way of accumulating non-linearly.

## How We Want It

Our goal in this tutorial is to rewrite this event handler so
that it __only__ uses data from arguments (coeffects!). This will take a few steps.

The first is that we switch to
using `reg-event-fx` (instead of `reg-event-db`).

Event handlers registered via `reg-event-fx` are slightly
different to those registered via `reg-event-db`. `-fx` handlers
get two arguments, but the first is not `db`. Instead it
is an argument which we will call `cofx` (that's a nice distinct
name which will aid communication).

Previous tutorials showed there's a `:db` key in `cofx`.  We
now want `cofx` to have other keys and values, like this:
```clj
(reg-event-fx                     ;; note: -fx
   :load-defaults
   (fn [cofx event]                 ;; cofx means coeffects
     (let [val (:local-store cofx)  ;; <-- get data from cofx
           db  (:db cofx)]          ;; <-- more data from cofx
       {:db (assoc db :defaults val)}))) ;; returns an effect
```

Notice how `cofx` magically contains a `:local-store` key with the
right value. Nice! But how do we make this magic happen?

## Abracadabra

Each time an event is "handled", a brand new `context` (map)
is created, and within that `context` is a `:coeffects` key which
is a further map (initially empty).

That pristine `context` value (containing, in turn, a pristine `:coeffects` map) is threaded
through the `:before` function of each interceptor (in the event handler chain)
before it finally reaches the registered event handler, which sits on the end of the chain,
itself wrapped up in an interceptor. We know this story well from a previous tutorial.

These `:before` functions have the
opportunity to `assoc` into the `:coeffects` map (within the `context`), cumulatively adding to what it holds.
Later, our event handler, which sits on the end of the chain, finds that its first
 `cofx` argument contains just the right data, like, for example, a value for the key `:local-store`.
So, it is the event handler's Interceptor chain which can add to the "world" eventually
"seen" by an event handler.

## Which Interceptors?

If Interceptors put data in `:coeffects`, then we'll need to add the right ones
when we register our event handler.

Something like this (this handler is the same as before, except for one detail):
```clj
(reg-event-fx
   :load-defaults
   [ (inject-cofx :local-store "defaults-key") ]     ;; <-- this is new
   (fn [cofx event]
     (let [val (:local-store cofx)
           db  (:db cofx)]
       {:db (assoc db :defaults val)})))
```

Look at that - my event handler now has a new Interceptor which will inject (assoc) the
right key/value pair (`:local-store`)
into `context's` `:coeffects`, which itself is the map which goes on to be the first argument
to our event handler (aka `cofx`).

## `inject-cofx`

`inject-cofx` is part of the re-frame API.

It is a function which returns an Interceptor whose `:before` function loads
a key/value pair into a `context's` `:coeffects` map.

`inject-cofx` takes either one or two arguments. The first is always the `id` of the coeffect
required (called a `cofx-id`). The 2nd is an optional additional value.

So, in the case above, the `cofx-id` was `:local-store`  and the additional value
was "defaults-key" which was presumably the LocalStore key.

## More `inject-cofx`

Here's some other usage examples:

  -  `(inject-cofx :random-int 10)`
  -  `(inject-cofx :guid)`
  -  `(inject-cofx :now)`

I could create an event handler which has access to 3 coeffects:
```clj
(reg-event-fx
    :some-id
    [(inject-cofx :random-int 10) (inject-cofx :now)  (inject-cofx :local-store "blah")]  ;; 3
    (fn [cofx _]
       ... in here I can access cofx's keys :now :local-store and :random-int))
```

But that's probably just greedy.

And so, to the final piece in the puzzle: how does `inject-cofx`
know what to do when it is given `:now` or `:local-store`?
Each `cofx-id` requires a different action.

## Meet `reg-cofx`

This function is also part of the re-frame API.

It allows you to associate a `cofx-id` (like `:now` or `:local-store`) with a
handler function that injects the right key/value pair.

The function you register will be passed two arguments:

  - a `:coeffects` map (to which it should add a key/value pair), and
  - optionally, the additional value supplied to `inject-cofx`

and it is expected to return a modified `:coeffects` map.

## Example Of `reg-cofx`

Above, we wrote an event handler that wanted `:now` data to be available.  Here
is how a handler could be registered for `:now`:
```clj
(reg-cofx               ;; registration function
   :now                 ;; what cofx-id are we registering
   (fn [coeffects _]    ;; second parameter not used in this case
      (assoc coeffects :now (js.Date.))))   ;; add :now key, with value
```

The outcome is:

  1. because that cofx handler above is now registered for `:now`, I can
  2. add an Interceptor to an event handler which
  3. looks like `(inject-cofx :now)`
  4. which means within that event handler I can access a `:now` value from `cofx`

As a result, my event handler is pure.

## Another Example Of `reg-cofx`

This:
```clj
(reg-cofx               ;; new registration function
   :local-store
   (fn [coeffects local-store-key]
      (assoc coeffects
             :local-store
             (js->clj (.getItem js/localStorage local-store-key)))))
```


With these two registrations in place, I could now use both `(inject-cofx :now)` and
`(inject-cofx :local-store "blah")` in an event handler's interceptor chain.

To put this another way:  I can't use `(inject-cofx :blah)` UNLESS I have previously
used `reg-cofx` to register a handler for `:blah`. Otherwise `inject-cofx` doesn't
know how to inject a `:blah`.

## Secret Interceptors

In a previous tutorial we learned that `reg-events-db`
and `reg-events-fx` add default interceptors to the front of the interceptor chain
specified during registration. We found they inserted an Interceptor called `do-fx`.

I can now reveal that
they also add `(inject-cofx :db)` at the front of each chain.

Guess what that injects into the `:coeffects` of every event handler? This is how `:db`
is always available to event handlers.

Okay, so that was the last surprise. Now you know everything.

If ever you wanted to use DataScript, instead of an atom-containing-a-map
like `app-db`, you'd replace `reg-event-db` and `reg-event-fx` with your own
registration functions and have them auto insert the DataScript connection.


## Testing

During testing, you may want to stub out certain coeffects.

You may, for example, want to test that an event handler works
using a specific `now`.

In your test, you'd mock out the cofx handler:
```clj
(reg-cofx
   :now
   (fn [coeffects _]
      (assoc coeffects :now (js/Date. 2016 1 1)))   ;; then is `:now`
```

If your test does alter registered coeffect handlers, and you are using `cljs.test`,
then you can use a `fixture` to restore all coeffects at the end of your test:
```clj
(defn fixture-re-frame
  []
  (let [restore-re-frame (atom nil)]
    {:before #(reset! restore-re-frame (re-frame.core/make-restore-fn))
     :after  #(@restore-re-frame)}))

(use-fixtures :each (fixture-re-frame))
```

`re-frame.core/make-restore-fn` creates a checkpoint for re-frame state (including
registered handlers) to which you can return.

## The 5 Point Summary

In note form:

  1. Event handlers should only source data from their arguments
  2. We want to "inject" required data into the first, cofx argument
  3. We use the `(inject-cofx :key)` interceptor in registration of the event handler
  4. It will look up the registered cofx handler for that `:key` to do the injection
  5. We must have previously registered a cofx handler via `reg-cofx`
````

## File: docs/dominoes-30k.md
````markdown
Previously, we looked at the dominoes from 60,000 feet. We will now shift
down to 30,000 feet and look in more detail, this time with code fragments.

<img align="right" width="360px" src="../images/Readme/todolist.png">
## The Setup



Imagine this: our app displays a list of items.

The user clicks the "delete" button next to the 3rd item in the list.

In response, let's track what happens within our imaginary re-frame app? Let's manually step through
the resulting six domino cascade.

!!! note ""
    Don't expect
    to completely grok the terse code presented below. We're still at 30,000 feet. More details later.

## Domino 1 - Event Dispatch

In order for it to be clicked, that 3rd delete button must have already been rendered. And rendering
in re-frame is done by a `ViewFunction`. Perhaps it was rendered like this:
```clj
(defn delete-button
  [item-id]
  [:div.garbage-bin
    :on-click #(re-frame.core/dispatch [:delete-item item-id])])
```

It is that `on-click` handler (function) which interests us. When the user clicks on the garbage-bin icon, that function is called.
```clj
#(re-frame.core/dispatch [:delete-item item-id])
```
This function calls re-frame's `dispatch` to emit an `event`.

Every re-frame `event` is a vector and, in this case, the dispatched `event` has two elements:
```clj
[:delete-item 2486]
```
where `2486` is an `id` I just made up for that 3rd item.

The first element of an event vector,
`:delete-item`, is the kind of event. The rest is optional data, salient to the
`event`.

Events express user intent in a domain-specific way.
They are the language of your re-frame system.

## Domino 2 - Event Handling

An `event handler` (function), which we'll name `h`, is now called to
compute the `effect` of the event `[:delete-item 2486]`.

On startup, re-frame apps register handlers for events using `reg-event-fx`. So,
in our imaginary app, because `h` is the handler function for `:delete-item` events,
it must have been registered like this:
```clj
(re-frame.core/reg-event-fx   ;; a part of the re-frame API
  :delete-item                ;; the kind of event
  h)                          ;; the handler function for this kind of event
```

Because `h` is an event handler, it is written to take two arguments:

  1. a `coeffects` map. This data describes the current state of "the world". In the simplest case, it is a trivial map like this: `{:db a-value}` where `a-value` is the current application state held in `app-db`.
  2. the `event` to handle, which would be `[:delete-item 2486]` in this case.

`h` will compute effects as data. That means: it computes how the world should be changed
by the event, and it returns a map of `effects` which describe the necessary changes.

Here's a sketch (we are at 30,000 feet):
```clj
(defn h                          ;; maybe choose a better name like `delete-item`
 [coeffects event]               ;; `coeffects` holds the current state of the world
 (let [item-id  (second event)   ;; extract id from event vector
       db       (:db coeffects)  ;; extract the current application state
       new-db   (dissoc-in db [:items item-id])]   ;; new app state
   {:db new-db}))                ;; a map of the necessary effects
```

There are ways (described in later tutorials) for you to inject necessary aspects
of "the world" into that first `coeffects` argument (map). Different
event handlers need to know different "things" about the world to do their job. But
current "application state" is one aspect of the world which is
invariably needed, and it is available by default in the `:db` key. So
the current value in `app-db` is available via the expression `(:db coeffects)`.

The value returned by `h` is a map with only one key, like this:
```clj
{:db new-db}     ;; `new-db` is the newly computed application state
```
So, `h` computes one effect, and returns it. And that effect says to make a change to
application state.

Please pay particular attention to this overall flow, within `h`:

  1. `h` obtains the current application state (a map) via `(:db coeffects)`
  2. it computes a modified application state via `(dissoc-in db [:items item-id])`
  3. it returns this modified application state in an effects map `{:db new-db}`


BTW, here is a more idiomatic (and terser) rewrite of `h` using [destructuring](https://clojure.org/guides/destructuring) of the args:
```clj
(defn h
  [{:keys [db]} [_ item-id]]    ;; <--- new: obtain db and item-id directly
  {:db  (dissoc-in db [:items item-id])})    ;;
```


## Domino 3 - Effect Handling

`effect handler` functions action the `effects` returned by `h`.

In Domino 2, `h` returned:
```clj
{:db  new-db}   ;; `new-db` is the new, computed application state
```

Each key of this returned map identifies one kind
of `effect`, and the value for that key supplies further details.
The map returned by `h` only has one key, `:db`, so it is specifying only one effect.

On startup, a re-frame app can register `effects handlers` using `reg-fx`. For example,
the effect handler function for the `:db` effect could be registered like this:
```clj
(re-frame.core/reg-fx       ;; part of the re-frame API
  :db                       ;; the effects key
  (fn [val]                 ;; the handler function for the effect
    (reset! app-db val)))   ;; put the new value into the ratom app-db
```

Just to be clear, this `reset!` of `app-db` is a mutative, effectful action. That's
what effect handlers do. They change the world. They are not pure functions.

Now, you don't need to ever register an effects handler for `:db`
because re-frame supplies one built in. re-frame manages `app-db` and so it
will look for any changes (effects) to it.

But if, instead, `h` had returned:
```clj
{:wear  {:pants "velour flares"  :belt false}
 :tweet "Okay, yes, I am Satoshi. #coverblown"}
```
Then, the two effects handlers registered for `:wear` and `:tweet` would
be called to action those two effects. And, no, re-frame
does not supply standard effect handlers for either, so you would need to have
written them yourself, and then registered them.

For example:
```clj
(re-frame.core/reg-fx    ;; re-frame API
  :wear        ;; the effects key which this handler can action
  (fn [val]    ;; val would be, eg, {:pants "velour flares"  :belt false}
    ...))      ;; do what's necessary to action the side effect
```

## Domino 4 - Query

The action of updating `app-db` (in Domino 3) will trigger the `v = f(s)` part of the flow.

The application state
`s` has just changed (in Domino 3) and now boom, boom go Dominoes 4, 5,
and 6, at the end of which we have a new view, `v`, being shown to the user.

In this domino 4, a query (function) over this app state is automatically
called.  This query function "extracts" data from application state, and
then computes "a materialised view" of the application state - producing
data which is useful to the view functions in domino, 5.

Now, in this particular case, the query function is pretty trivial.
Because the items are stored in app state, there's not a lot
to compute and, instead, it acts like a simple extractor or accessor,
just plucking the list of items out of application state:
```clj
(defn query-fn
  [db v]         ;; db is the current value in app-db, v the query vector
  (:items db))   ;; not much of a materialised view
```

On program startup, such a `query-fn` must be associated with a `query-id`,
(so it can be used via `subscribe` in domino 5) using `re-frame.core/reg-sub`,
like this:
```clj
(re-frame.core/reg-sub  ;; part of the re-frame API
   :query-items         ;; query id
   query-fn)            ;; function to perform the query
```
Which says "if, in domino 5, you see a `(subscribe [:query-items])`, then
call `query-fn` to compute it".

## Domino 5 - View

Because the query function for `:query-items` just re-computed a new value,
any view (function) which uses a `(subscribe [:query-items])`
is called automatically (reactively) to re-compute new DOM (in response to a change in its source data).

View functions compute a data structure, in hiccup format, describing
the DOM nodes required. In this "items" case, the view functions will *not* be generating
hiccup for the just-deleted item obviously but, other than this,
the hiccup computed "this time" will be the same as "last time".

```clj
(defn items-view
  []
  (let [items  (subscribe [:query-items])]  ;; source items from app state
    [:div (map item-render @items)]))   ;; assume item-render already written
```

Notice how `items` is "sourced" from "app state" via `re-frame.core/subscribe`.
It is called with a vector argument, and the first element of that vector is
a query-id which identifies the "materialised view" required by the view.

Note: `subscribe` queries can be parameterised. So, in real-world apps
you might have this:<br>
  `(subscribe [:items "blue"])`

The vector identifies, first, the query, and then
supplies further arguments. You could think of that as
representing `select * from Items where colour="blue"`.

Except there's no SQL available and you would be the one to implement
the more sophisticated `query-fn` capable of handling the
"where" argument. More in later tutorials.

## Domino 6 - DOM

The hiccup returned by the view function
is made into real browser DOM by Reagent/React. No code from you required. Just happens.

The DOM computed "this
time" will be the same as "last time", **except** for the absence of DOM for the
deleted item, so the mutation will be to remove those now-missing
DOM nodes from the browser.

## 3-4-5-6 Summary

The key point to understand about our 3-4-5-6 example is:

  - a change to app state ...
  - triggers query functions to rerun ...
  - which triggers view functions to rerun
  - which causes modified browser DOM

Boom, boom, boom go the dominoes. It is a reactive data flow.

## Aaaaand we're done

At this point, the re-frame app returns to a quiescent state,
waiting for the next event.


## Two Sub-Cascades

You might have noticed that there's actually two sub-cascades 1-2-3 and 4-5-6, and they have a similar structure.

In each, it is the second to last domino which
computes "data descriptions" of the changes required, and it is
the last domino which does the dirty work and actions these descriptions.

But you seldom need to worry yourself about the dirty work dominos. re-frame
mostly takes care of them for you.


> One is only fruitful at the price of being rich in oppositions <br>
>
>   -- Nietzsche, Twilight of the Idols

Pragmatically, in functional systems, the most interesting part is how and when you arrange to not be pure.
````

## File: docs/Effects.md
````markdown
Maybe 20% of the time, event handlers need to cause side effects.

This tutorial explains how side effects are actioned,
how you can create your own side effects, and how you can
make side effects a noop in testing and event replays.

## Where Effects Come From

When an event handler is registered via `reg-event-fx`, it must return effects. Like this:
```clj
(reg-event-fx              ;; -fx registration, not -db registration
  :my-event
  (fn [cofx [_ a]]        ;; 1st argument is coeffects, instead of db
    {:db       (assoc (:db cofx) :flag  a)
     :fx       [[:dispatch [:do-something-else 3]]]})) ;; return effects
```

`-fx` handlers return a description of the side-effects required, and that description is a map.

## The Effects Map

An effects map contains instructions.

Each key/value pair in the map is one instruction - the key uniquely identifies
the particular side effect required, and the value for that key provides
further data. The type of value depends on the specific side-effect.

Here's the two instructions from the example above:
```clj
{:db       (assoc db :flag  a)         ;; side effect on app-db
 :fx       [[:dispatch [:do-something-else 3]]]}     ;; dispatch this event
```

The `:db` key instructs that "app-db" should be `reset!` to the
value supplied.

And the `:fx` key instructs that an ordered list of other effects should be
executed. In this case a `:dispatch` key instructs that an event should be
dispatched. The value is the vector to dispatch.

There are many other possible
effects, like for example `:dispatch-later`, `dispatch-n`, `:set-local-store`, etc.

And so on. And so on. Which brings us to a problem.

## Infinite Effects

Although re-frame supplies a number of built-in effect handlers, the set of
possible effects is open ended.

What if you use PostgreSQL and want an effect which issues mutating
queries?  Or what if you want to send logs to Logentries or metrics to DataDog.
Or write values to `windows.location`. Or save cookies.

The list of effects is long and varied, with everyone needing to use a
different combination.

So effect handling has to be extensible. You need a way to define
your own side effects.

## Extensible Side Effects

re-frame provides a function `reg-fx` through which you can register
your own `Effect Handlers`. Use it like this:
```clj
(reg-fx         ;; <-- registration function
   :butterfly   ;;  <1> effect key
   (fn [value]  ;;  <2> effect handler
      ...
      ))
```

__<1>__  __Effect Key__  Later, when an effects map contains
the key `:butterfly`, the function we are registering will be used to action it.

__<2>__  __Effect handler__ The function which actions the side effect. Later, it will be called
with one argument - the value associated with the key in the effects map.

So, if an event handler returned these two effects:
```clj
{:dispatch   [:save-maiden 42]
 :butterfly  "Flapping"}         ;; butterfly effect, but no chaos !!
```

Then the function we registered for `:butterfly` would be called to handle
that effect. And it would be called with the parameter "Flapping".

So, terminology:

- `:butterfly` is an "effect key"
- and the function registered is an "effect handler".

So re-frame has both `event` handlers and `effect` handlers and they are
different, despite them both starting with `e` and ending in `t`!!

## Writing An Effect Handler

A word of advice - make them as simple as possible, and then
simplify them further.  You don't want them containing any fancy logic.

Why?  Well, because they are all side-effecty they will be a pain
to test rigorously. And the combination of fancy logic and limited
testing always ends in tears.  If not now, later.

A second word of advice - when you create an effect handler,
you also have to design (and document!) the structure of the
`value` expected.

When you do, realise that you are designing a nano DSL for `value` and try to
make that design simple too. If you resist being terse and smart, and instead, favor slightly
verbose and obvious, your future self will thank you. Create as little
cognitive overhead as possible for the eventual readers of your effectful code.

Right. So, this advice coming from the guy who named effects `fx` ... Oh, the hypocrisy.

In my defence, here's the built-in effect handler for `:db`:
```clj
(reg-fx
  :db
  (fn [value]
    (reset! re-frame.db/app-db value)))
```

So, yeah, simple ... and, because of it, I can almost guarantee there's no bug in ... bang, crash, smoke, flames.

!!! note
    The return value of an effect handler is ignored.

## :db Not Always Needed

An effects map does not need to include the `effect key` `:db`.

It is perfectly valid for an event handler
to not change `app-db`.

In fact, it is perfectly valid for an event handler to return
an effects map of `{}`.  Slightly puzzling, but not a problem.

## What Makes This Work?

A silently inserted interceptor.

Whenever you register an event handler via __either__ `reg-event-db`
or `reg-event-fx`, an interceptor, cunningly named `do-fx`,
is inserted at the beginning of the chain.

Example: if your event handler registration looked like this:
```clj
(reg-event-fx
  :some-id
  [debug (path :right)]     ;; <-- two interceptors, apparently
  (fn [cofx _]
     {})                    ;; <-- imagine returned effects here
```

While it might look like you have registered with 2 interceptors,
`reg-event-fx` will make it 3:
```clj
[do-fx debug (path :right)]
```

It silently inserts `do-fx` at the front, and this is a good thing.

The placement of `do-fx` at the beginning of the interceptor chain means
its `:after` function would be the final act when the chain is executed
(forwards and then backwards, as described in the Interceptor Tutorial).

In this final act, the `:after` function extracts `:effects` from `context`
and simply iterates across the key/value pairs it contains, calling the
registered "effect handlers" for each.

!!! Note "For the record"
    The FISA Court requires that we deny all claims
    that `do-fx` is secretly injected NSA surveillance-ware. <br>
    We also note that you've been sloppy with your personal
    grooming again, including, but not limited to, forgetting to clean your teeth on one occasion last week.

If ever you want to take control of the way effect handling is done,
create your own alternative to `reg-event-fx` and, in it, inject
your own version of the `do-fx` interceptor at the front
of the interceptor chain.  It is only a few lines of code.


## Order Of Effects?

***Prior to v1.1.0***, the answer is: no guarantees were provided about ordering. Actual order is an implementation detail upon which you should not rely.

***From v1.1.0 onwards***, two things changed:

  - re-frame guaranteed that the `:db` effect will always be actioned first, if present. But other than that, no guarantee is given for the other effects.
  - a new effect called `:fx` was added, and it provides a way for effects to be ordered.

In fact, with v1.1.0 ***best practice changed*** to event handlers should only return two effects `:db` and `:fx`, in which case `:db` was always done first and then `:fx`, and within `:fx` the ordering is sequential. This new approach is more about making it easier to compose event handlers from many smaller functions, but more specificity around ordering was  a consequence.

## Effects With No Data

Some effects have no associated data:
```clj
(reg-event-fx
  :some-id
  (fn [coeffect _]
     {:exit-fullscreen nil}))    ;;   <--- no data, use a nil
```

In these cases, although it looks odd, just supply `nil` as the value for this key.

The associated effect handler would look like:
```clj
(reg-fx
  :exit-fullscreen
  (fn [_]             ;; we don't bother with that nil value
     (.exitFullscreen js/document)))
```

## Testing And Noops

When you are running tests or replaying events, it is sometimes
useful to stub out effects.

This is easily done - you simply register a noop effect handler.

Want to stub out the `:dispatch` effect?  Do this:
```clj
(reg-fx
  :dispatch
  (fn [_] ))    ;; a noop
```

If your test does alter registered effect handlers, and you are using `cljs.test`,
then you can use a `fixture` to restore all effect handlers at the end of your test:
```clj
(defn fixture-re-frame
  []
  (let [restore-re-frame (atom nil)]
    {:before #(reset! restore-re-frame (re-frame.core/make-restore-fn))
     :after  #(@restore-re-frame)}))

(use-fixtures :each (fixture-re-frame))
```

`re-frame.core/make-restore-fn` creates a checkpoint for re-frame state (including
registered handlers) to which you can return.

## Existing Effect Handlers

`re-frame's` built-in effect handlers, like `dispatch-n` and `dispatch-later`, are detailed in [the API](https://day8.github.io/re-frame/api-builtin-effects/) document.

And please review the [External-Resources document](https://day8.github.io/re-frame/External-Resources/) for a list of 3rd party Effect Handlers.

## Summary

The 4 Point Summary in note form:

1. Event handlers should only return a description of required effects
2. They return a map like `{:effect1 value1 :effect2 value2}`
3. Keys of this map can refer to builtin effect handlers (see below) or custom ones
4. We use `reg-fx` to register our own effect handlers, built-in ones are already registered
````

## File: docs/flow-mechanics.md
````markdown
> In a rush? You can get away with skipping this page on the first pass.


This tutorial explains the underlying reactive mechanism used in dominoes 4-5-6. It goes on to introduce `re-frame.core/reg-sub-raw`.

## On Flow

Arguments from authority ...

> Everything flows, nothing stands still.   (Panta rhei)

> No man ever steps in the same river twice for it's not the same river and he's not the same man.

[Heraclitus 500 BC](http://en.wikiquote.org/wiki/Heraclitus). Who, being Greek, had never seen a frozen river. [alt version](http://farm6.static.flickr.com/5213/5477602206_ecb78559ed.jpg).


> Think of an experience from your childhood. Something you remember clearly, something you can see,
feel, maybe even smell, as if you were really there. After all you really were there at the time,
weren’t you? How else could you remember it? But here is the bombshell: you weren’t there. Not a
single atom that is in your body today was there when that event took place .... Matter flows
from place to place and momentarily comes together to be you. Whatever you are, therefore, you
are not the stuff of which you are made. If that does not make the hair stand up on the back of
your neck, read it again until it does, because it is important.

Steve Grand


### How Flow Happens In Reagent

To implement a reactive flow, Reagent provides a `ratom` and a `reaction`.
re-frame uses both of these building blocks, so let's now make sure we understand them.

`ratoms` behave just like normal ClojureScript atoms. You can `swap!` and `reset!` them, `watch` them, etc.

From a ClojureScript perspective, the purpose of an atom is to hold mutable data.  From a re-frame
perspective, we'll tweak that paradigm slightly and **view a `ratom` as having a value that
changes over time.**  Seems like a subtle distinction, I know, but because of it, re-frame sees a
`ratom` as a Signal.

A Signal is a value that changes over time.  So it is a stream of values. Each time a ratom gets
`reset!` that's a new value in the stream.

The 2nd building block, `reaction`, acts a bit like a function. It's a macro which wraps some
`computation` (a block of code) and returns a `ratom` holding the result of that `computation`.

The magic thing about a `reaction` is that the `computation` it wraps will be automatically
re-run  whenever 'its inputs' change, producing a new output (return) value.

Eh, how?

Well, the `computation` is just a block of code, and if that code dereferences one or
more `ratoms`, it will be automatically re-run (recomputing a new return value) whenever any
of these dereferenced `ratoms` change.

To put that yet another way, a `reaction` detects a `computation's` input Signals (aka input `ratoms`)
and it will `watch` them, and when, later, it detects a change in one of them,  it will re-run that
computation, and it will `reset!` the new result of that computation into the `ratom` originally returned.

So, the `ratom` returned by a `reaction` is itself a Signal. Its value will change over time when
the `computation` is re-run.

So, via the interplay between `ratoms` and `reactions`,  values 'flow' into computations and out
again, and then into further computations, etc.  "Values" flow (propagate) through the Signal graph.

But this Signal graph must be without cycles, because cycles cause mayhem!  re-frame achieves
a unidirectional flow.

Right, so that was a lot of words. Some code to clarify:

```Clojure
(ns example1
 (:require-macros [reagent.ratom :refer [reaction]])  ;; reaction is a macro
 (:require        [reagent.core  :as    reagent]))

(def app-db  (reagent/atom {:a 1}))           ;; our root ratom  (signal)

(def ratom2  (reaction {:b (:a @app-db)}))    ;; reaction wraps a computation, returns a signal
(def ratom3  (reaction (condp = (:b @ratom2)  ;; reaction wraps another computation
                             0 "World"
                             1 "Hello")))

;; Notice that both computations above involve de-referencing a ratom:
;;   - app-db in one case
;;   - ratom2 in the other
;; Notice that both reactions above return a ratom.
;; Those returned ratoms hold the (time varying) value of the computations.

(println @ratom2)    ;; ==>  {:b 1}       ;; a computed result, involving @app-db
(println @ratom3)    ;; ==> "Hello"       ;; a computed result, involving @ratom2

(reset!  app-db  {:a 0})       ;; this change to app-db, triggers re-computation
                               ;; of ratom2
                               ;; which, in turn, causes a re-computation of ratom3

(println @ratom2)    ;; ==>  {:b 0}    ;; ratom2 is result of {:b (:a @app-db)}
(println @ratom3)    ;; ==> "World"    ;; ratom3 is automatically updated too.
```

So, in FRP-ish terms, a `reaction` will produce a "stream" of values over time (it is a Signal),
accessible via the `ratom` it returns.

## Components  (view functions)

When using Reagent, your primary job is to write one or more `components`.
This is the view layer.

Think about `components` as `pure functions` - data in, Hiccup out.  `Hiccup` is
ClojureScript data structures which represent DOM. Here's a trivial component:

```Clojure
(defn greet
  []
  [:div "Hello ratoms and reactions"])
```

And if we call it:
```Clojure
(greet)
;; ==>  [:div "Hello ratoms and reactions"]
```

You'll notice that our component is a regular Clojure function, nothing special. In this case, it takes
no parameters and it returns a ClojureScript vector (formatted as Hiccup).

Here is a slightly more interesting (parameterised) component (function):
```Clojure
(defn greet                    ;; greet has a parameter now
  [name]                       ;; 'name' is a ratom  holding a string
  [:div "Hello "  @name])      ;; dereference 'name' to extract the contained value

;; create a ratom, containing a string
(def fw (reagent/atom "re-frame"))

;; call our `component` function, passing in a ratom
(greet fw)
;; ==>  [:div "Hello " "re-frame"]    returns a vector
```

So components are easy - at core they are a render function which turns data into
Hiccup (which will later become DOM).

Now, let's introduce `reaction` into this mix.  On the one hand, I'm complicating things
by doing this, because Reagent allows you to be ignorant of the mechanics I'm about to show
you. (It invisibly wraps your components in a `reaction` allowing you to be blissfully
ignorant of how the magic happens.)

On the other hand, it is useful to understand exactly how the Reagent Signal graph is wired.

```Clojure
(defn greet                ;; a component - data in, Hiccup out.
  [name]                   ;; name is a ratom
  [:div "Hello "  @name])  ;; dereference name here, to extract the value within

(def fw (reagent/atom "re-frame"))

;; The computation '(greet fw)' returns Hiccup which is stored into 'hiccup-ratom'
(def hiccup-ratom  (reaction (greet fw)))    ;; <-- use of reaction !!!

;; what is the result of the initial computation ?
(println @hiccup-ratom)
;; ==>  [:div "Hello " "re-frame"]    ;; returns hiccup  (a vector of stuff)

;; now change 'fw'
;; 'fw' is an input Signal for the reaction above.
;; Warning: 'fw' is not an input signal because it is a parameter. Rather, it is
;; because 'fw' is dereferenced within the execution of the reaction's computation.
;; reaction notices what ratoms are dereferenced in its computation, and watches
;; them for changes.
(reset! fw "blah")            ;;    fw changes

;; The reaction above will notice the change to 'fw' ...
;; ... and will re-run its computation ...
;; ... which will have a new "return value"...
;; ... which will be "reset!" into "hiccup-ratom"
(println @hiccup-ratom)
;; ==>   [:div "Hello " "blah"]    ;; yep, there's the new value
```

So, as `fw` changes value over time (via a `reset!`), the output of the computation `(greet fw)`
changes, which in turn means that the value in `hiccup-ratom` changes. Both `fw` and
`hiccup-ratom` are FRP Signals. The Signal graph we created causes data to flow from
`fw` into `hiccup-ratom`.

Derived Data, flowing.


### Truth Interlude

I haven't been entirely straight with you:

 1. Reagent re-runs `reactions` (re-computations) via requestAnimationFrame. So a
re-computation happens about 16ms after an input Signals change is detected, or after the
current thread of processing finishes, whichever is the greater. So if you are in a REPL
and you run the lines of code above one after the other too quickly,  you might not see the
re-computation done immediately after `fw` gets reset!, because the next animationFrame
hasn't run (yet).  But you could add a `(reagent.core/flush)` after the reset! to force
re-computation to happen straight away.

 2. `reaction` doesn't actually return a `ratom`.  But it returns something that has
ratom-nature, so we'll happily continue believing it is a `ratom` and no harm will come to us.

On with the rest of my lies and distortions...


### reg-sub-raw

This low level part of the API provides a way to register a subscription handler - so
the intent is similar to `reg-sub`.

You use it like other registration functions:
```clj
(re-frame.core/reg-sub-raw   ;; it is part of the API
  :query-id     ;; later use (subscribe [:query-id])
  some-fn)      ;; this function provides the reactive stream
```

The interesting bit is how `some-fn` is written. Here's an example:
```clj
(defn some-fn
  [app-db event]    ;; app-db is not a value, it is a reagent/atom
  (reaction (get-in @app-db [:some :path])))  ;; returns a reaction
```
Notice:

  1. `app-db` is a reagent/atom. It is not a value like `reg-sub` gets.
  2. it returns a `reaction` which does a computation. It does not return a value like `reg-sub` does.
  3. Within that `reaction` `app-db` is deref-ed (see use of `@`)

As a result of point 3, each time `app-db` changes, the wrapped `reaction` will rerun.
`app-db` is an input signal to that `reaction`.

Unlike `reg-sub`, there is no 3-arity version of `reg-sub-raw`, so there's no way for you to provide an input signals function.
Instead, even simpler, you can just use `subscribe` within the `reaction` itself. For example:
```clj
(defn some-fn
   [app-db event]
   (reaction
     (let [a-path-element @(subscribe [:get-path-part])]   ;; <-- subscribe used here
       (get-in @app-db [:some a-path-element]))))
```
As you can see, this `reaction` has two input signals: `app-db` and `(subscribe [:get-path-part])`.  If either changes,
the `reaction` will rerun.

In some cases, the returned `reaction` might not even
use `app-db` and, instead, it might only use `subscribe` to provide input signals. In that case, the
registered subscription would belong to "Layer 3" of the signal graph (discussed in earlier tutorials).

Remember to deref any use of `app-db` and `subscribe`.  It is a rookie mistake to forget. I do it regularly.

Instead of using `reaction` (a macro), you can use `reagent/make-reaction` (a utility function) which gives you the additional
ability to attach an `:on-dispose` handler to the returned reaction, allowing you to do cleanup work when the subscription is no longer needed.
[See an example of using `:on-dispose` here](Subscribing-To-External-Data.md)

### Example reg-sub-raw

The following use of `reg-sub` can be found in [the todomvc example](https://github.com/day8/re-frame/blob/master/examples/todomvc/src/todomvc/subs.cljs):
```clj
(reg-sub
  :visible-todos

  ;; signal function - returns a vector of two input signals
  (fn [query-v _]
    [(subscribe [:todos])
     (subscribe [:showing])])

  ;; the computation function - 1st arg is a 2-vector of values
  (fn [[todos showing] _]
    (let [filter-fn (case showing
                      :active (complement :done)
                      :done   :done
                      :all    identity)]
      (filter filter-fn todos))))
```

we could rewrite this use of `reg-sub` using `reg-sub-raw` like this:
```clj
(reg-sub-raw
  :visible-todos
  (fn [app-db event]  ;; app-db not used, name shown for clarity
    (reaction         ;; wrap the computation in a reaction
      (let [todos   @(subscribe [:todos])   ;; input signal #1
            showing @(subscribe [:showing]) ;; input signal #2
            filter-fn (case showing
                        :active (complement :done)
                        :done   :done
                        :all    identity)]
        (filter filter-fn todos))))
```

A view could do `(subscribe [:visible-todos])` and never know which of
the two variations above was used. Same result delivered.
````

## File: docs/flows-advanced-topics.md
````markdown
> Have you been to Clevelinnati? No? Clevelinnati can be found at the geometric midpoint between Cleveland and Cincinnati. What's that, you say it's not on the map?
Well, no worries. Now that I've defined Clevelinnati for you, you'll know exactly how to get to get there... As long as Cleveland stays still.

Previously, we explored the capabilities of [flows](/re-frame/Flows).
We discussed a flow which derives the area of a room from its dimensions:

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result data-cm-doc-no-eval-on-init>
{:id     :kitchen-area
 :inputs {:w [:kitchen :width]
          :h [:kitchen :length]}
 :output (fn [{:keys [w h]}] (* w h))
 :path   [:kitchen :area]
 :live-inputs {:tab [:tab]}
 :live?  (fn [{:keys [tab]}]
           (= tab :kitchen))}
</div>

Here, we'll consider the nuances of re-frame's reactive model, and how flows stand to solve some of its fundamental shortcomings.

Introducing yet another demo app! Turns out, we were measuring the kitchen to fill it with balloons. It's a balloon prank planner app. Consider the following:

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-eval-on-init data-cm-doc-no-edit data-cm-doc-no-result>
(rf/reg-sub
 ::kitchen-area
 (fn [db _] (get-in db [:kitchen :area])))

(rf/reg-sub
 ::kitchen-height
 (fn [db _] (get-in db [:kitchen :height])))

(rf/reg-sub
 ::kitchen-volume
 (fn [_] [(rf/subscribe [::kitchen-area]) (rf/subscribe [::kitchen-height])])
 (fn [[area height] _] (* area  height)))

(rf/reg-sub
 ::num-balloons-to-fill-kitchen
 (fn [_] [(rf/subscribe [::kitchen-volume])])
 (fn [[volume] _] (let [std-balloon-volume 2.5]
                   (/ volume std-balloon-volume))))

(rf/reg-event-fx
 ::order-ballons-for-kitchen-prank
 (fn [{:keys [balloons-per-bag db] :as cofx} _]
   (let [num-balloons-to-fill-kitchen :???     ;; How can I get this value???
         num-bags-to-buy (js/Math.ceil
                          (/ num-balloons-to-fill-kitchen
                             balloons-per-bag))]
     {:fx [[:amazon {:fn :order
                     :sku :balloon-bag
                     :ct num-bags-to-buy}]]})))
</div>

How can we get a correct value for `num-balloons-to-fill-kitchen`?
You might try calling `(rf/subscribe [::num-balloons-to-fill-kitchen])`, but re-frame comes back with a warning about reactive context,
and memory leaks... oh my!

### Reactive context

To know if a thing has changed, you have to remember what it was.
To propagate change from one identity to another, you have to remember their relationship (a [`watchable`](https://clojuredocs.org/clojure.core/add-watch)).
Memory is state. Remembering is a side-effect.

Reagent does this. Its main constructs - *reactive atom*, and *component* - are stateful, impure.
We depend on this memory. It abstracts the essential complexity of reactive programming.

Reagent manages atoms and components with an event loop. Only in the context of this loop can we be sure reagent's memory is consistent.
Literally, this is called [`*ratom-context*`](https://github.com/reagent-project/reagent/blob/a14faba55e373000f8f93edfcfce0d1222f7e71a/src/reagent/ratom.cljs#L12).

Generally, `*ratom-context*` only has value during the evaluation of a component function (i.e., at "render time").
When `*ratom-context*` has no value, reactive atoms behave differently.

You can simply call [`reagent.ratom/reactive?`](http://reagent-project.github.io/docs/master/reagent.ratom.html#var-reactive.3F)
to find out whether your code is running in a reactive context.

#### Reactive context in re-frame

Here's where re-frame enters the picture:

- An **event handler** is a pure function, with no reactive context.
- A **subscription handler** is pure, too.
- A **subscription**, on the other hand, is a reactive atom.
- Calling `subscribe` has the side-effect of *creating* a **subscription**.

Outside of a reactive context, a subscription's behavior differs:
Not only the behavior of the reactive atom, but also its [caching](#caching) behavior.

#### Reactive context in your app

Subscriptions and handlers differ in purity and runtime context.
This means they have a [coloring problem](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/).

We [express some business logic with subscriptions](https://github.com/day8/re-frame/issues/753), and some with events.
This introduces the coloring problem to our business domain.

We can ignore the problem in [some cases](https://github.com/day8/re-frame/issues/740#issuecomment-955749230),
but the essential consequence of calling `subscribe` in an event handler is an unsafe cache.
Calling `subscribe` allocates physical memory on the client, and re-frame has no way to deallocate it.
This puts us back in C territory.

Instead, to safely get a value for `num-balloons-to-fill-kitchen`, we have to duplicate the business logic that we wrote into our subscription,
along with the *entire* subgraph of subscription inputs:

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result data-cm-doc-no-eval-on-init>
(rf/reg-event-fx
 ::order-ballons-for-kitchen-prank
 (fn [{:keys [balloons-per-bag db] :as cofx} _]
   (let [kitchen-area (get-in db [:kitchen :area])
         kitchen-height (get-in db [:kitchen :height])
         kitchen-volume (* area height)      ;; eyelids start drooping here
         std-balloon-volume 2.5
         num-balloons (/ kitchen-volume std-balloon-volume)
         num-bags-to-buy (js/Math.ceil
                          (/ num-balloons balloons-per-bag))]
     {:fx [[:amazon {:fn :order
                     :sku :balloon-bag
                     :ct num-bags-to-buy}]]})))
</div>

Not only have we [drenched](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) our code, but now it has to do all our calculations twice.

Of course you can design around the problem, but at what cost?
We sympathize with you developers, for the hours you may have spent poring over an event handler, just to re-write the code as a subscription, and vice-versa.

### Caching

Subscriptions have a built-in caching mechanism, which stores the value as long as there is a component in the render tree which uses it.
Basically, when components call `subscribe` with a particular `query-v`, re-frame sets up a callback.
When those components unmount, this callback deletes the stored value.
It removes the subscription from the graph, so that it will no longer recalculate.
This is a form of [reference counting](https://en.wikipedia.org/wiki/Reference_counting) - once the last subscribing component unmounts, then the subscription is freed.

This often works as intended, and nothing gets in our way.
It's elegant in a sense - a view requires certain values, and those values only matter when the view exists. And vice versa.
But when these values are expensive to produce or store, their existence starts to matter.
The fact that some view is creating and destroying them starts to seem arbitrary.
Subscriptions don't *need* to couple their behavior with that of their calling components.

The easy, automatic lifecycle behavior of subscriptions comes with a coupling of concerns. You can't directly control this lifecycle.
You have to control it by proxy, by mounting and unmounting your views. You can't *think* about your signal graph without thinking about views first.

The `app-db` represents your business state, and signals represent outcomes of your business logic. Views are just window dressing.
We're tired of designing our whole business to change every time we wash the windows!

### Paths

A [layer-2](/re-frame/subscriptions/#the-four-layers) subscription basically *names* an `app-db` path.
What does a layer-3 subscription *name*?

A materialized view of data, or a derived value.

Subscriptions occupy their own semantic territory, separate from `app-db`.
Only within view functions (and other subscriptions) can we access this domain.
Outside of views, they form an impenetrable blob.

So, re-frame is simple. `app-db` represents and *names* the state of your app.
Except, so does this network of subscription names. But you can't always *use* those, only sometimes.

### Signal graph state

Here's the story we like to tell about re-frame:

- **User Actions** cause **Events**
- **Events** cause **Effects**
- **Effects** cause **State Changes**
- **State changes** cause **View Rendering**

Turns out, it's not so simple.
Not only do *state changes* cause *view rendering*, but *view rendering* also causes *state changes*.
Specifically, render logic changes the state of subscriptions.

Your app's actual story might go something like this:

> An event fires. A subscription runs. An outer component passes new props to an inner one. A reaction runs. Another reaction runs. A component unmounts. That subscription disposes, clearing its cache.

Sound [familiar](https://github.com/day8/re-frame/discussions/776)?

> Then, the same event fires. Another component mounts. The same subscription runs. Its calculation is heavy, so your app lags every time.
> "Wait," you ask, "what happened to the cache? Why is my subscription recalculating when its inputs are the same?"
> After a lot of headscratching, you find out it's because the subscription itself is not the same. Render logic caused it to dispose, and a new one took its place.

This isn't a bug, nor is it inevitable, but in our experience, complexity adds up fast.
Once Reagent, Re-frame and React begin to share the concern of reactive dataflow, they can race, or play chicken.
I'll react if you do! Can't run me if I unmount you first! Can't unmount me if I run you first!

When a view calls `subscribe`, it creates a reaction. When that view unmounts, it frees the reaction.
These are side-effects on the signal graph
(that is, the graph of all subscriptions which are actively re-calculating their output when their inputs change, and storing that value).

```
event -> app-db -> signals -> view -> event
                                  ∟-> signal graph -> signals -> view
```

Something is looping in on itself here:

- The value of `app-db` determines when a view lives or dies.
- *And*, the value of a view determines when a signal lives or dies.
- *And*, the value of a signal determines when a view lives or dies.

If views derive solely from `app-db`, then why must signals derive from views?
Why not simply have *everything* derive from `app-db`?

`event -> app-db -> signal graph -> signals -> view -> event`

### A better way

Here's the good news about [flows](/re-frame/Flows):

__You can access a flow's output value any time, anywhere,__
since flows are controlled by re-frame/interceptors, not reagent/reactions.
Instead of thinking about reactive context, just think about the outcome of the latest event.
If you know `app-db`, you know your flow value.
You can also [subscribe to flows](/re-frame/Flows/#subscribing-to-flows).

__If you know a flow's name, you know its location,__
since flows store their output in `app-db`, at a static path.
It doesn't matter what other flows & paths it depends on.
The value you need simply stays where you put it.

__A flow's lifecycle is a pure function of `app-db`__.
That means you explicitly define when a flow lives, dies, is registered or cleared.
You do this directly, not via your component tree.

Like many Clojure patterns, flows are *both* nested *and* flat.
Even though `::num-balloons-to-fill-kitchen` depends on other flows, we can access it directly:

<div class="cm-doc" data-cm-doc-no-edit data-cm-doc-no-result data-cm-doc-no-eval-on-init>
(rf/reg-flow
 {:id ::kitchen-volume
  :inputs {:area [:kitchen :area]
           :height [:kitchen :height]}
  :output (fn [{:keys [area height]}]
            (* area height))
  :path [:kitchen :volume]})

(rf/reg-flow
 {:id ::num-balloons-to-fill-kitchen
  :inputs {:volume (rf/flow<- ::kitchen-volume)}
  :output (fn [{:keys [volume]}]
            (let [std-balloon-volume 2.5]
               (/ volume std-balloon-volume)))})

(rf/reg-event-fx
 ::order-ballons-for-kitchen-prank
 (fn [{:keys [balloons-per-bag] :as cofx} _]
   (let [num-balloons (rf/get-flow db ::num-balloons-to-fill-kitchen) ;; easy!
         num-bags-to-buy (js/Math.ceil
                          (/ num-balloons
                             balloons-per-bag))]
     {:fx [[:amazon {:fn :order
                     :sku :balloon-bag
                     :ct num-bags-to-buy}]]})))
</div>
````

## File: docs/Flows.md
````markdown
> This is an experimental, proposed feature for re-frame.
> We'd love to hear your feedback!
> Please join our discussions on [github](https://github.com/day8/re-frame/discussions/795) and [slack](https://clojurians.slack.com/archives/C073DKH9P/p1698792674379499).

---

This tutorial introduces **Flows**, part of [Domino 3](http://localhost:8000/re-frame/dominoes-30k/#domino-3-effect-handling) (effects).

!!! Note "Not to be confused with..."
    - [re-frame-async-flow-fx](https://github.com/day8/re-frame-async-flow-fx). A `re-frame/flow` is totally synchronous, running on every event.
    - The [on-changes interceptor](/re-frame/api-re-frame.core/#on-changes). Flows are an evolution of this idea.
    - [domino](https://github.com/domino-clj/domino). Another take on dataflow programming, inspired by re-frame.

## What are flows?

A **flow** describes how to derive a value from other values.
When one part of your app state changes, another part changes in response.
More concretely, when the values change at one or more paths within `app-db`,
then the value at another path is "automatically" recalculated.

## Why do we need flows?
We turn to flows when we need a dynamic relationship between values - a ["difference which makes a difference"](http://faculty.washington.edu/jernel/521/Form.htm).

For instance, how would you model this problem?

- a `length` and a `width` make an `area`
- changing either value changes the `area`
- deleting either value deletes the `area`
- a bad value invalidates the `area`
- leaving the page deletes the `area`

In re-frame, [data coordinates functions](/re-frame/re-frame/).
Here, we need multiple data sources (`length`, `width`) to coordinate a single function (`area`).
A subscription could do this, but with [caveats](/re-frame/flows-advanced-topics#reactive-context).

We think flows offer a [Better Way](/re-frame/flows-advanced-topics#a-better-way), both simpler and more practical.

!!! Note "The DataFlow Paradigm"
    Dataflow programming emerged in the 1970s, so it is almost as foundational as functional programming.
    Indeed, reactive programming - so much the rage these days - is simply a subset of dataflow programming.
    In contrast with imperative building blocks like `if/then`, `next` and `goto`,
    dataflow programming implements control flow via the propagation of change.
    Both the functional and dataflow paradigms have profoundly influenced the design of re-frame.
    Hence, `re-frame's` tagline: "derived data, flowing".

## A Basic Flow

Here's a basic `flow`. It describes how to derive the area of a room from its dimensions:

<div class="cm-doc" data-cm-doc-no-result>
{:id     :room-area
 :inputs {:w [:room :width]
          :h [:room :length]}
 :output (fn calc-area [{:keys [w h] :as inputs}]
           (* w h))
 :path   [:room :area]}
</div>

- **`:id`** - uniquely identifies this flow.
- **`:inputs`** - a map of `app-db` paths to observe for change.
- **`:output`** - calculates the new derived value.
    - Takes a map of resolved inputs.
    - Simply takes `app-db` if there are no inputs.
- **`:path`** - denotes *where* the derived value should be stored.

On every event, when the values at `:inputs` change, `:output` is run, and the result is stored in `app-db` at `:path`.

## A Basic Example

To show `Flows` in action, let's do some live coding.
First, we add the necessary `requires` (`reg-flow` is still in the `alpha` namespace):

<div class="cm-doc">
(ns re-frame.example.flows
  (:require [re-frame.alpha :as rf]
            [reagent.dom.client :as rdc]))
</div>

And, here's the code for our app: the user can enter `height` and `width` values and, in response, they see `area`:

<div class="cm-doc">
(rf/reg-sub      :width  (fn [db [_ room]]    (get-in db [room :width])))
(rf/reg-sub      :length (fn [db [_ room]]    (get-in db [room :length])))
(rf/reg-event-db :inc-w  (fn [db [_ room]] (update-in db [room :width] inc)))
(rf/reg-event-db :inc-h  (fn [db [_ room]] (update-in db [room :length] inc)))
(rf/reg-event-db :init   (fn [db [_ room]] (-> db
                                              (update :kitchen merge {:width 10 :length 15})
                                              (update :garage merge {:width 20 :length 20}))))

(def clickable
  {:cursor "pointer" :border "2px solid grey" :user-select "none"})

(defn room-form [room]
  [:form
   [:h4 room " calculator"]
   "width:"
   @(rf/subscribe [:width room])
   [:span {:style clickable
           :on-click #(rf/dispatch [:inc-w room])} "+"]
   [:br]
   "length:"
   @(rf/subscribe [:length room])
   [:span {:style clickable
           :on-click #(rf/dispatch [:inc-h room])} "+"]])
</div>

## Registering a flow

Now the interesting part, we use `reg-flow`:

<div class="cm-doc" data-cm-doc-result-format="pass-fail">
(rf/reg-flow
  {:id     :garage-area
   :inputs {:w [:garage :width]
            :h [:garage :length]}
   :output (fn [{:keys [w h]}] (* w h))
   :path   [:garage :area]})
</div>

!!! Note "Arity-2 version"
    In addition to `(reg-flow flow)`, you can also call `(reg-flow id flow)`.
    This gives it a signature just like the usual `reg-event-` and `reg-sub` calls.
    Our example would look like `(reg-flow :garage-area {...})`.

We write a subscription for the flow's output `:path`:

<div class="cm-doc">
(rf/reg-sub
 :area
 (fn [db [_ room]] (get-in db [room :area])))
</div>

And, we use this subscription in a view:

<div class="cm-doc">
(defn app-container [& children]
  (into [:div {:style {:padding "1rem"
                       :border  "2px solid grey"}}]
        children))

(defn room-calculator [room]
  [:div
   [room-form room]
   " Area:"
   @(rf/subscribe [:area room])])  ;;  <--- subscribing

(rf/dispatch-sync [:init])

(defonce garage-calculator-root
  (rdc/create-root (js/document.getElementById "garage-calculator")))

(rdc/render garage-calculator-root
            [app-container [room-calculator :garage]])
</div>

<div id="garage-calculator"></div>

## How does it work?

`event handlers` yield `effects`. Typically, they yield a `:db` effect, causing a new value of `app-db`.
But first, re-frame updates your `:db` effect by running each registered `flow`.

!!! Note "Caution: implicit behavior ahead"
    Here, the tradeoff becomes clear. A `flow` can change `app-db` implicitly.
    This means the `:db` effect which you express in your event handlers may not match the actual `app-db` you'll get as a result.

Re-frame achieves this using an [interceptor](/re-frame/Interceptors/). Here's what it does:

- Destructure the current `app-db`, resolving the paths in `:inputs`
    - this yields a value like `{:w 10 :h 15}`.
- Destructure the *previous* `app-db` as well, to see if any of these values have changed.
    - For instance, if it sees `{:w 11 :h 24}`, that means the inputs have changed.
    - `{:w 10 :h 15}` would mean no change.
- *If* the inputs have changed:
    - Call the `:output` function, passing it the previous result, and the current `:inputs`.
    - Store the newly derived value (in this case, `150`) in `app-db`, at the `:path`.

Isn't that remarkable? What, you say it's *unremarkable?* Well, that's even better.

## Remarks

Reality check. Here's why this basic flow might not excite you:

### Can't I just use events?

> Re-frame can already set values. Events were the one true way to update `app-db`. Why invent _another_ mechanism for this?

In this sense, they are redundant. Rather than use a flow, you could simply call a `derive-area` within each relevant event:

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result>
(defn derive-area [{:keys [width length] :as room}]
  (assoc room :area (* width length)))

(rf/reg-event-db
 :width
 (fn [db [_ w]]
   (-> db
       (assoc-in [:garage :width] w)
       (update :garage derive-area))))
</div>

This works just fine... *or does it*? Actually, we forgot to change the `:length` event. Our area calculation will be wrong every time the user changes the length! Easy to fix, but the point is that we had to fix it at all. How many events will we need to review? In a mature app, this is not a trivial question.

*Design is all tradeoffs*. Flows allow us to say "This value simply derives from these inputs. It simply changes when they do." We do this at the expense of some "spooky action at a distance" - in other words, we accept that no particular event will be responsible for that change.

### Are flows just reactions?

You might notice a similarity with [reagent.core/reaction](https://reagent-project.github.io/docs/master/reagent.core.html#var-reaction).
Both yield an "automatically" changing value.

Reagent controls *when* a reaction updates, presumably during the evaluation of a component function.
Flows, on the other hand, are part of [re-frame time](/re-frame/on-dynamics/#re-frame-time), running every time an `event` occurs.

When a component derefs a reaction, that component knows to re-render when the value changes.

You can't deref a flow directly. It doesn't emit a value directly to any caller.
Instead, it emits a new version of `app-db`. The rest of your app reacts to `app-db`, not your flow.

### But really, why do I need flows?

Some apps do complex tasks, with deep layers of branching and looping.
But most apps do simple things, as well.
Many such tasks amount to synchronization - maintaining an invariant within a changing data structure.

And of course, a task which seems complex may just be a chain of simple tasks.

One relatable example is that of trying to maintain cascading error states. Imagine your UI has a validation rule: `start date` must be before `end date`.
After the user changes either value, the error state must be calculated.
The result indicates whether to enable the submit button or display an error message.

Now, imagine your UI has many validation rules, each with its own error state.
In this case, the submit button state is a secondary calculation which combines these error states.
Cascading, derived values.

Data flows from the leaves (what the user entered), through intermediate nodes (error predicate functions), through to the root (submit button state).
Both the intermediate values and the root value are important.

### Is this a rules engine?

You might be tempted to view `Flows` as having something to do with a rules engine, but it absolutely isn't that. It is simply a method for implementing dataflow. Each value is derivative of other values, with multiple levels of that process arranged in a tree structure in which many leaf values contribute to a terminal root value (think submit button state!).

### Can't I just use subscriptions?

You could derive your garage's area with a [layer-3 subscription](/re-frame/subscriptions/#the-four-layers):

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result>
(rf/reg-sub
 ::garage-area-sub
 (fn [_] [(subscribe [:width]) (subscribe [:length])])
 (fn [[w h] _] (* w h)))
</div>

Just like a `flow`, this subscription's value changes whenever the inputs change, and (obviously) you call `subscribe` to access that value.

A flow stores its `:output` value in `app-db`, while subscriptions don't. We designed re-frame on the premise that `app-db` holds your *entire* app-state.
Arguably, derived values belong there too. We feel there's an inherent reasonability to storing everything in one place.
It's also more practical (see [Reactive Context](/re-frame/flows-advanced-topics#reactive-context)).

Just like with layered subscriptions, one flow can use the value of another. Remember the `:inputs` to our first flow?

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result>
{:inputs {:w [:garage :width]
          :h [:garage :length]}}
</div>

## Layering flows

In the values of the `:inputs` map, vectors stand for paths in `app-db`.
The `flow<-` function, however, gives us access to *other flows*.

Here's a flow using two other flows as inputs: `::kitchen-area` and `::living-room-area`.
When either input changes value, our flow calls the `:output` function to recalculate its own value:

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result>
{:id     :main-room-ratio
 :inputs {:kitchen     (rf/flow<- ::kitchen-area)
          :living-room (rf/flow<- ::living-room-area)}
 :output (fn [{:keys [kitchen living-room]}]
           (/ kitchen living-room))
 :path   [:ratios :main-rooms]}
</div>

As before, once `:output` runs, the resulting value is stored at `:path`.
So, the new value of `app-db` will contain a number at the path `[:ratios :main-rooms]`

Under the hood, flows relate to each other in a dependency graph.
An input like `(rf/flow<- ::kitchen-area)` creates a dependency.
That means re-frame will always run `::kitchen-area` first,
ensuring its output value is current before your `:main-room-ratio` flow can use it.

!!! Note "Our dataflow model"
    Dataflow is often conceptualized as a graph.
    Data flows through edges, and transforms through nodes.
    Here's how our DSL articulates the traditional dataflow model:

    - `flow` - a map, serving as a node specification
    - `:id` - uniquely identifies a node
    - `:inputs` - a set of edges from other nodes
    - `flow<-` - declares another node id as an input dependency
    - `reg-flow` - creates a running node from a specification

    Crucially, the name `flow` isn't exactly short for "dataflow".
    A `flow` is a static value, specifying one possible segment of a dataflow graph.
    Dataflow is a [dynamic process](/re-frame/on-dynamics/#on-dynamics), not a value.
    Both the data and the graph itself can change over time.

    - Changing the data means running the flows which are currently registered.
    - Changing the graph is a matter of [registering and clearing](#redefining-and-undefining) flows.

## Subscribing to flows

In our examples so far, we've used a regular subscription, getting our flow's output path.
In `re-frame.alpha`, you can also subscribe to a flow by name.
This bypasses the [caching behavior](/re-frame/flows-advanced-topics#caching) of a standard subscription.

Here's how you can subscribe to our garage-area flow.
The stable way, with a query vector:

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result>
(re-frame.alpha/subscribe [:flow {:id :garage-area}])
</div>

And the experimental way, with a query map:

<div class="cm-doc" data-cm-doc-no-eval data-cm-doc-no-edit data-cm-doc-no-result>
(re-frame.alpha/sub :flow {:id :garage-area})
</div>

## Living and Dying

> Between death... and arising... is found an existence— a "body"... that goes to the place of rebirth. This existence between two realms... is called intermediate existence.

> -- *Vasubandhu, on [bardos](https://en.wikipedia.org/wiki/Bardo)*

In practice, some flows are too expensive to run all the time.
It can still be hard to predict when a flow will run, leading to defensive programming.
Sometimes we'd like to simply turn our flow off, so we can stop thinking about it.
For this, we use a `:live?` function.

The quote above deals with phenomenal life, but you can also think of `:live?` as in a tv or internet broadcast.
Data flows, but only when the `flow` itself is live.

Let's try it out. For example, here's a barebones tab picker, and something to show us the value of `app-db`:

<div class="cm-doc">
(def tabs [:kitchen :garage])

(def clickable-tab (into clickable {:padding "0.5rem"}))
(def active-tab (into clickable-tab {:color "green"}))

(rf/reg-sub :current-tab :-> :tab)
(rf/reg-event-db :change-tab (fn [db [_ tab]] (assoc db :tab tab)))

(defn tab [id current?]
  (let [current-tab (rf/sub :current-tab)]
    [:span {:style (if (= id @current-tab)
                     active-tab clickable-tab)
          :on-click #(rf/dispatch [:change-tab id])}
   (name id)]))

(defn tab-picker []
  (into [:div] (for [id tabs] [tab id])))

(defn tabbed-app []
  (let [current-tab (rf/sub :current-tab)]
    [:div
     [tab-picker @current-tab]
     (case @current-tab
       :kitchen [room-calculator :kitchen]
       :garage [room-calculator :garage]
       nil)]))
</div>

### Live?

Here's a more advanced version of our room calculator flow.

<div class="cm-doc" data-cm-doc-result-format="pass-fail">
(rf/reg-flow
 {:id     :kitchen-area
  :inputs {:w [:kitchen :width]
           :h [:kitchen :length]}
  :output (fn [{:keys [w h]}] (* w h))
  :path   [:kitchen :area]
  :live-inputs {:tab [:tab]}
  :live?  (fn [{:keys [tab]}]
            (= tab :kitchen))})
</div>

Notice the new `:live-inputs` and `:live?` keys.
Just like `:output`, `:live:?` is a function of the resolved `:live-inputs`.

Re-frame only calculates the `:output` when the `:live?` function returns a truthy value.
Otherwise, the flow is presumed dead.

Let's test it out:

<div class="cm-doc">
(defn debug-app-db []
  [:pre
   {:style {:font-size 12 :margin "1rem" :white-space "pre-wrap"}}
   (str @re-frame.db/app-db)])

(rf/dispatch-sync [:init])

(defonce tabbed-app-root (rdc/create-root (js/document.getElementById "tabbed-app")))

(rdc/render tabbed-app-root [app-container [debug-app-db]
                                           [tabbed-app]])
</div>

<div id="tabbed-app"></div>

Try switching tabs.
Notice how the path `[:kitchen :area]` only exists when you're in the `room-calculator` tab. What's happening here?

### Lifecycle

After handling an event, re-frame runs your flows. First, it evaluates `:live?`, using the new `app-db`.
Depending on the return value of `:live?`, re-frame handles one of 4 possible state transitions:

| transition | action |
|---|---|
| From **live** to **live** |  run `:output` (when `:inputs` have changed) |
| From **dead** to **live** |  run `:output` |
| From **live** to **dead** |  run `:cleanup` |
| From **dead** to **dead** |  do nothing |

Basically, *arising* flows get output, *living* flows get output as needed, and *dying* flows get cleaned up.

### Cleanup

A `:cleanup` function takes `app-db` and the `:path`, and returns a new `app-db`.

Try adding this `:cleanup` key into the `:kitchen-area` flow above (be sure to `eval` the code block again).

<div class="cm-doc" data-cm-doc-no-result>
:cleanup (fn [db path]
           (assoc-in db path :unknown!))
</div>

By default, `:cleanup` dissociates the path from `app-db`. By declaring this `:cleanup` key in your flow, you override that default function. Now, instead of removing `:area`, you set it to `:unknown!`.

Now, is this a good idea? After all, we might consider the area known, as long as we know the width and length. Maybe we should do no cleanup, and keep the value, even when `:live?` returns false. In that case, our `:cleanup` function would simply be: `:cleanup (fn [db _] db)`.

The point is, *you* express when the signal lives or dies, not your render tree.

## Redefining and Undefining

Not only do flows have a lifecycle (defined by `:live?` and `:cleanup`), but this lifecycle also includes registration and deregistration.

- When you call `reg-flow`, that flow comes alive.
    - `:output` runs, even if the inputs haven't changed.
    - That's because the flow itself has changed.
- When you call `clear-flow`, it dies (running `:cleanup`).
- Re-frame provides `:reg-flow` and `:clear-flow` [effects](#re-frame/Effects/) for this purpose.

Here's another demonstration. Think of it as a stripped-down todomvc.
You can add and remove items in a list:

<div class="cm-doc">
(rf/reg-sub :items :-> (comp reverse :items))

(rf/reg-event-db
 ::add-item
 (fn [db [_ id]] (update db :items conj id)))

(rf/reg-event-db
 ::delete-item
 (fn [db [_ id]] (update db :items #(remove #{id} %))))

(defn item [id] [:div "Item" id])

(defn items []
  (into [:div] (map item) @(rf/subscribe [:items])))

(defn controls []
  (let [id (or (apply max @(rf/subscribe [:items])) 0)]
    [:div
     [:span {:style clickable
             :on-click #(rf/dispatch [::add-item (inc id)])}
      "Add"] " "
     [:span {:style clickable
             :on-click #(rf/dispatch [::delete-item id])}
      "Delete"] " "]))

(defonce item-counter-basic-root
  (rdc/create-root (js/document.getElementById "item-counter-basic")))

(rdc/render item-counter-basic-root
            [app-container [controls] [items]])
</div>

<div id="item-counter-basic"></div>

Now, imagine your business adds some requirements:

- At least 1 item per person.
- No more than 3 items per person.

First things first, we express these requirements as data:

<div class="cm-doc">
(def base-requirements {:min-items 0 :max-items 3})
</div>

Then, we'll use a flow to evaluate which requirements are met.

!!! Note "State, not events"
    These requirements aren't about what *happens*, only what things *are*.
    It's your app *state* that matters, not any particular event or view.
    Our flow doesn't care how it happened that a requirement was met, nor what to do next.

For reasons that will become clear, let's write a [factory function](https://en.wikipedia.org/wiki/Factory_%28object-oriented_programming%29) for this flow.
It builds a flow that validates our item list against any given requirements:

<div class="cm-doc" data-cm-doc-result-format="pass-fail">
(defn error-state-flow [{:keys [min-items max-items] :as requirements}]
  {:id :error-state
   :path [:error-state]
   :inputs {:items [:items]}
   :output (fn [{:keys [items]}]
             (let [ct (count items)]
               (cond
                 (> ct max-items)  :too-many
                 (<= ct min-items) :not-enough
                 :else             :ok)))})
</div>

And let's register a flow that fits our base requirements:

<div class="cm-doc" data-cm-doc-result-format="pass-fail">
(rf/reg-flow (error-state-flow base-requirements))
</div>

Now this flow is calculating an error-state value, and adding it to `app-db` after every event.
This happens whenever `:items` have changed... right?
Actually, there's another way to make a flow recalculate - we can re-register it.

Let's update the app to display our new error state:

<div class="cm-doc">

(defn warning []
  (let [error-state (rf/sub :flow {:id :error-state})]
    [:div {:style {:color "red"}}
     (->> @error-state
          (get {:too-many   "Too many items. Please remove one."
                :not-enough "Not enough items. Please add one."
                :ok         [:br]}))]))

(rf/dispatch-sync [:init])

(defonce item-counter-error-root
  (rdc/create-root (js/document.getElementById "item-counter-error")))

(rdc/render item-counter-error-root
            [app-container [debug-app-db] [controls] [warning] [items]])
</div>

<div id="item-counter-error"></div>

Your app is working fine, until your next design meeting.
Now they want a way to change the max item limit.
A little contrived, I know. But not uncommon from a programming perspective.

Luckily, our flow factory can make a new flow for any requirement.
Therefore, putting in this feature is just a matter of triggering the `:reg-flow` effect.

We build a basic form to change the requirement:

<div class="cm-doc">
(defn requirement-picker []
  [:<>
   "Max items: "
   [:input
    {:style {:background "lightgrey"}
     :type "number"
     :on-change #(rf/dispatch
                  [:change-requirements
                   {:max-items (-> % .-target .-value)}])}]])
</div>

And a corresponding event, which triggers our `:reg-flow` effect:

<div class="cm-doc" data-cm-doc-result-format="pass-fail">
(rf/reg-event-fx
 :change-requirements
 (fn [_ [_ new-requirements]]
   {:fx [[:reg-flow (error-state-flow (merge base-requirements new-requirements))]]}))
</div>

What happens after `:reg-flow` runs? Are there now two flows? Actually, no.

- If you register a new flow with the same `:id`, it replaces the old one.
- When we trigger `[:reg-flow (error-state-flow ...)]`:
    - The old `:error-state` flow runs `:cleanup`
    - The new `:error-state` flow runs `:output`

Not only does changing the inputs lead to new output, but so does changing the flow itself.
Let's test it out:

<div class="cm-doc">
(rf/dispatch-sync [:init])

(defonce item-counter-requirements-root
  (rdc/create-root (js/document.getElementById "item-counter-requirements")))

(rdc/render item-counter-requirements-root
            [app-container [debug-app-db] [controls] [requirement-picker] [warning] [items]])
</div>

<div id="item-counter-requirements"></div>
````

## File: docs/Interceptors.md
````markdown
This tutorial explains re-frame `Interceptors`. Until now, they have been a hidden detail
but, as we are about to find out, they are both important and useful.

## Why Interceptors?

There's two reasons.

1. We want __simple event handlers__.
   Interceptors allow us nicely to look after "cross-cutting" concerns like undo, tracing
   and validation.  They help us to factor out commonality, hide complexity and introduce
   further steps into the "Derived Data, Flowing" story promoted by re-frame.
   So, you'll want to use Interceptors because they solve problems, and help you to write nice code.

2. Under the covers, Interceptors provide **the** mechanism underneath Domino 2 (event handling)
   and Domino 3 (effect handling). They are a central concept and understanding them better will serve you well, even if you don't need to directly use them very often.

## What Do Interceptors Do?

They wrap.

Specifically, they wrap event handlers.

Imagine your event handler is like a piece of ham. An interceptor would be
like bread on either side of your ham, which makes a sandwich.

And two Interceptors, in a chain, would be like you put another
pair of bread slices around the outside of the existing sandwich to make
a sandwich of the sandwich. Now it is a very thick sandwich.

Interceptors wrap on both sides of an event handler, layer after layer.

## Wait, I know That Pattern!

Interceptors implement `middleware`, but differently.

Traditional `middleware` - often seen in web servers - creates a data
processing pipeline (to process arriving HTTP requests) via the nested composition of higher order functions.
The result is a "stack" of functions. Data representing a request flows through this pipeline,
first forwards from one end to the other, and then backwards (as the response).

Interceptors achieve the same outcome by assembling functions, as data,
in a collection - a chain of interceptors, rather than a stack of middleware.
Data can then be iteratively
pipelined, first forwards through the functions in the chain,
and then backwards along the same chain.

Because the interceptor pipeline is composed via a collection (data!), rather than
higher order functions, it is a more flexible arrangement.

## What's In The Pipeline?

Data. It flows through the pipeline being progressively transformed.

Fine. But what data?

With a web server, the middleware "stack" progressively
transforms an `HTTP request` in one direction, and, then in the backwards
sweep, it progressively produces a `response`.

In re-frame, the forward sweep progressively creates `coeffects`
(inputs to the event handler that you write), while the backwards sweep processes the `effects`
(outputs from the event handler you write).

I'll pause while you read that sentence again. That's the key
concept, right there.

## Show Me

At the time when you register an event handler, you can provide a chain of interceptors too.

Using a 3-arity registration function:
```clj
(reg-event-db
   :some-id
   [in1 in2]       ;; <-- a chain of 2 interceptors (a vector of 2 interceptors)
   (fn [db v]      ;; <-- the handler here, as before
      ....)))
```

!!! note ""
    Each Event Handler can have its own tailored interceptor chain, provided at registration-time.

## Handlers Are Interceptors Too

You could see that registration above as associating `:some-id` with
two things:

1. a chain of 2 interceptors `[in1 in2]`
2. an `event handler`.

Except, the `event handler` is turned into an interceptor too (we'll see how shortly).

So, ***actually***, `:some-id` is only associated with one thing: a 3-chain of interceptors,
with the `event handler` wrapped in an interceptor, called say `ih`, and put on the
end of the other two, forming a vector of three interceptors: `[in1 in2 ih]`.

But wait, there's more. The registration function itself, `reg-event-db`, actually takes this 3-chain
and inserts its own standard interceptors, called say `std1` and `std2`
(which do useful things, again more soon) at the front,
so **ACTUALLY**, there's about 5 interceptors in the chain: `[std1 std2 in1 in2 ih]`

So, ultimately, that event registration associates the event id `:some-id`
with __just__ a chain of interceptors. Nothing more.

Later, when a `(dispatch [:some-id ...])` happens, that 5-chain of
interceptors will be "executed".  And that's how an event gets handled.


## The Links Of The Chain

Each interceptor has this form:
```clj
{:id      :something             ;; decorative only - can be ignored
 :comment {...}                  ;; also decorative, optional
 :before  (fn [context] ...)     ;; returns a possibly modified `context`
 :after   (fn [context] ...)}    ;; returns a possibly modified `context`
```

That's essentially a map of two functions (a before function and an after function). Now imagine a vector of these maps - that's an interceptor chain.

Above we imagined an interceptor chain of `[std1 std2 in1 in2 ih]`. Now we know that this is really
a vector of 5 maps: `[{...} {...} {...} {...} {...}]`  where each of the 5 maps have
a `:before` and `:after` fn.

Sometimes, the `:before` and `:after` functions are noops - they take a `context` and return that `context` unchanged - think `identity`.

To "execute" an interceptor chain:

  1. create a `context` (which is a `map` with a certain structure, described below)
  2. iterate forwards over the chain, calling the `:before` function on each interceptor, threading `context` through each call.
  3. iterate over the chain in the opposite (backwards) direction calling the `:after` function on each interceptor and threading `context` through all the calls

Remember that the last interceptor in the chain is the `event handler` itself (wrapped up to be the `:before`).

That's it. That's how an event gets handled. This is how Dominoes 2 and 3 happen.


## What Is Context?

Some data called a `context` is threaded through all the calls.

This value is passed as the argument to every `:before` and `:after`
function and it is returned by each function, possibly modified.

A `context` is a map with this structure:
```clj
{:coeffects {:event [:some-id :some-param]
             :db    <original contents of app-db>}

 :effects   {:db    <new value for app-db>
             :dispatch  [:an-event-id :param1]}

 :queue     <a collection of further interceptors>
 :stack     <a collection of interceptors already walked>}
```

`context` has a `:coeffects` key and an `:effects` key which, if this was a web
server, would be somewhat analogous to `request` and `response`
respectively.

`:coeffects` will contain the inputs required by the event handler
(sitting presumably on the end of the chain). So that's
data like the `:event` being processed, and the initial state of `db`.

The handler-returned side effects are put into `:effects` including,
but not limited to, a new value for `app-db`.

The first few interceptors in a chain (the ones inserted by `reg-event-db`)
have `:before` functions which __prime__ the `:coeffects`
by adding in `:event`, and `:db`.  Of course, other interceptors can
add further to `:coeffects`.  Perhaps the event handler needs
data from localstore, or a random number, or a
DataScript connection. Interceptors can accumulate interesting information into
`:coeffects`, via their `:before` function.

Equally, some interceptors in the chain will have an `:after` function
which processes the side effects accumulated into `:effects`
including, but not limited to, updates to `app-db`.

## Threading the Context

Above, we imagined an interceptor chain like: `[std1 std2 in1 in2 ih]`.
One way to imagine the whole event handling process would be to see it written like this:

```clj
 ;; start by creating a context map
(let [context {:coeffects {}  :effects {} ...}]

  (-> context
    ;; Thread `context` through all the `:before` functions.
    ;; This phase is usually concerned with building up `:coeffects`
    ((:before std1) )    ;; noop
    ((:before std2) )    ;; adds `:event` and `:db` to `:coeffects`
    ((:before in1) )
    ((:before in2) )
    ((:before ih) )      ;; Domino 2 - handler called & return value put into `:effects`

    ;; Now backwards through the `:after` functions
    ;; This phase is usually concerned with building up or processing `:effects`
    ;; But could involve side effects like logging, or undo/redo state actions, etc
    ((:after  ih) )      ;; noop
    ((:after  in2) )
    ((:after  in1) )
    ((:after  std2) )    ;; noop
    ((:after  std1) )    ;; Domino 3 - all the `:effects` are processed
```

## Infographics

<a href="../images/interceptors.png">
  <img src="../images/interceptors.png">
</a>

## Self Modifying

There's something not shown in the above schematic.
Through both stages (the `:before` sweep and the `:after` sweep),
`context` **also** contains a `:queue` key which is the queue
interceptors yet to be processed, and a `:stack` key which is vector of
of the interceptors already done.

In advanced cases, these two values, within a `context`, can be modified by the
`:before` and `:after` functions through which the `context` is threaded.

So interceptors can be dynamically added
and removed from `:queue` and `:stack` by Interceptors already in the chain.

## Self Documenting

An interceptor has a required `:id` key, and an optional `:comment` key.
Re-frame itself does not take any special action in response to these keys.
Instead, they can offer situational awareness to you or your tools.
For instance, the [standard interceptor](#appendix-built-in-interceptors)
__path__ adds a `:comment` describing _which_ path it goes to.

## Credit

> All truths are easy to understand once they are discovered <br>
>   -- Galileo Galilei

This elegant and flexible arrangement was originally
designed by the [Pedestal Team](https://github.com/pedestal/pedestal/blob/master/guides/documentation/service-interceptors.md). Thanks!

## Let's Write An Interceptor

Dunno about you, but I'm easily offended by underscores.

Imagine we had a view which did this:
```clj
(dispatch [:delete-item 42])
```

We'd have to write this event handler:
```clj
(reg-event-db
  :delete-item
  (fn
     [db [_ key-to-delete]]     ;;  <---- Arrgggghhh underscore
     (dissoc db key-to-delete)))
```

Do you see it there? That `_` in the event destructuring!!! Almost mocking us with that
passive aggressive, understated thing it has going on!! Co-workers
have said I'm "being overly sensitive", perhaps even pixel-ist, but
you can see it too, right? Of course you can.

What a relief it would be to not have it there, but how? We'll write an interceptor: `trim-event`

Once we have written `trim-event`, our registration will change to look like this:
```clj
(reg-event-db
  :delete-item
  [trim-event]                ;;  <--- interceptor added
  (fn
     [db [key-to-delete]]     ;;  <---yaaah!  no leading underscore
     (dissoc db key-to-delete)))
```

`trim-event` will need to change the `:coeffects` map (within `context`).  Specifically, it will be
changing the `:event` value within the `:coeffects`.

`:event` will start off as `[:delete-item 42]`, but will end up `[42]` by the time it is supplied to the handler.
`trim-event`  will remove that
leading `:delete-item` because, by the time the event is
being processed, we already know what `id` it has.

And, so here it is:
```clj
(def trim-event
  (re-frame.core/->interceptor
    :id      :trim-event
    :before  (fn [context]
               (let [trim-fn (fn [event] (-> event rest vec))]
                 (update-in context [:coeffects :event] trim-fn)))))
```

As you read this, look back to what a `context` looks like.

Notes:

  1. We use the API function `->interceptor` to create an interceptor (which is just a map)
  2. Our interceptor only has a `:before` function
  3. Our `:before` is given `context`.  It modifies it and returns it.
  4. There is no `:after` for this Interceptor. It has nothing to do
     with the backwards processing flow of `:effects`. It is concerned only
     with `:coeffects` in the forward flow.

## Wrapping Handlers

We're going well. Let's do an advanced wrapping.

Earlier, in the "Handlers Are Interceptors Too" section, I explained that `event handlers`
are wrapped in an Interceptor and placed on the end of an Interceptor chain.  Remember the
whole `[std1 std2 in1 in2 ih]` thing?

We'll now look at the `ih` bit. How does an `event handler` get wrapped to be an Interceptor?

Reminder - there's two kinds of event handler:

   - the `-db` variety registered by `reg-event-db`
   - the `-fx` variety registered by `reg-event-fx`

I'll now show how to wrap the `-db` variety.

Reminder: here's what a `-db` handler looks like:
```clj
(fn [db event]               ;; takes two params
  (assoc db :flag true))     ;; returns a new db
```

So, we'll be writing a function which takes a `-db` handler as an argument, and returns an
Interceptor which wraps that handler:
```clj
(defn db-handler->interceptor
  [db-handler-fn]
  (re-frame.core/->interceptor     ;; an API function supplied by re-frame
    :id     :db-handler            ;; ids are decorative only
    :before (fn [context]          ;; this interceptor only has a `:before
              (let [{:keys [db event]} (:coeffects context)    ;; extract db and event from coeffects
                    new-db (db-handler-fn db event)]           ;; call the event handler
                 (assoc-in context [:effects :db] new-db)))))) ;; put db back into :effects, as `:db` effect
```

Notes:

  1.  Notice how this wrapper extracts data from the `context's` `:coeffects`
      and then calls the handler with that data  (a handler must be called with two args: `db` and `event`)
  2.  Equally notice how this wrapping takes the return value from `db-handler-fn`
      handler and puts it into `context's` `:effects`
  3.  The modified `context` (it has a new `:effects`) is returned
  3.  This is all done in `:before`.  There is no `:after` (it is a noop).  But this
      could have been reversed with the work happening in `:after` and `:before` a noop. Shrug.
      Remember that this Interceptor will be on the end of a chain.

Feeling confident?  Try writing the wrapper for `-fx` handlers - it is just a small variation.

## Summary

In this tutorial, we've learned:

__1.__ When you register an event handler, you can supply a collection of `Interceptors`:
```clj
 (reg-event-db
    :some-id
    [in1 in2]       ;; <-- a chain of 2 interceptors
    (fn [db v]      ;; <-- real handler here
       ....)))
```

__2.__ When you are registering an event handler, you are associating an event id with a chain of interceptors including:

  - the ones you supply (optional)  `in1` and `in2`
  - an extra one on the end, which wraps the event handler itself  (we called it `ih`)
  - a couple at the beginning of the chain `std1` & `std2`, put there by the `reg-event-db` or `reg-event-fx`.
  - the entire interceptor chain might end up a vector of 5 - `[std1 std2 in1 in2 ih]`

__3.__ An Interceptor Chain is executed in two stages. First a forwards sweep in which
  all `:before` functions are called, and then second, a backwards sweep in which the
  `:after` functions are called. A `context` map will be threaded through all these calls.
  An Interceptor chain is a reified "call stack".

__4.__ Interceptors do interesting things to `context`:

   - add to `:coeffects`  (data inputs to the handler)
   - process side `:effects` (returned by a handler)
   - produce logs
   - further process

In the next Tutorial, we'll look at (side) Effects in more depth.  Later again, we'll look at Coeffects.

## Appendix - Built-in Interceptors

re-frame comes with some built-in Interceptors:

  - __debug__: log each event as it is processed. Shows incremental [`clojure.data/diff`](https://clojuredocs.org/clojure.data/diff) reports.
  - __trim-v__:  a convenience. More readable handlers.

And some Interceptor factories (functions that return Interceptors):

  - __enrich__:  perform additional computations (validations?), after the handler has run. More derived data flowing.
  - __after__: perform side effects, after a handler has run.  Eg: use it to report if the data in `app-db` matches a schema.
  - __path__:  a convenience. Simplifies our handlers. Acts almost like `update-in`, to get and set a subtree of the app-db. Stores the path of the subtree in its `:comment`.

In addition, [a Library like re-frame-undo](https://github.com/day8/re-frame-undo) provides an Interceptor
factory called `undoable` which checkpoints app state.


To use them, first require them:
```Clojure
(ns my.core
  (:require
    [re-frame.core :refer [debug path]])
```
````

## File: docs/Loading-Initial-Data.md
````markdown
## Bootstrapping Application State

To bootstrap a re-frame application, you need to:

1. register handlers:

   - subscription  (via `reg-sub`)
   - events (via `reg-event-db` or `reg-event-fx`)
   - effects (via `reg-fx`)
   - coeffects (via `reg-cofx`)
2. kickstart reagent (views)
3. Load the right initial data into `app-db` which might, for example, be a `merge` of:

   - Some default values
   - Values stored in LocalStorage
   - Values obtained via service calls to server

Point 3 is the interesting bit and will be the main focus of this page,
but let's work our way through them ...

## 1. Register Handlers

re-frame's various handlers all work in the same way.  You declare
and register your handlers in the one step, like this "event handler" example:
```clj
(re-frame/reg-event-db       ;; event handler will be registered automatically
  :some-id
  (fn [db [_ value]]
    ...  do some state change based on db and value ))
```

As a result, there's nothing further you need to do because
handler registration happens as a direct result of loading the code
(presumably via a `<script>` tag in your HTML file).

## 2. Kick Start Reagent

Create a function `main` which does a `reagent/render` of your root reagent component `main-panel`:

```clj
(defn main-panel       ;; my top level reagent component
  []
  [:div "Hello DDATWD"])

(defn ^:export main     ;; call this to bootstrap your app
  []
  (reagent.dom/render [main-panel]
                  (js/document.getElementById "app")))
```

Mounting the top level component `main-panel` will trigger a cascade of child
component creation.  The full DOM tree will be rendered.

## 3. Loading Initial Data

Let's rewrite our `main-panel` component to use a subscription. In effect,
we want it to source and render some data held in `app-db`.

First, we'll create the subscription handler:
```Clojure
(re-frame.core/reg-sub     ;; a new subscription handler
  :name               ;; usage (subscribe [:name])
  (fn [db _]
    (:display-name db)))  ;; extracts `:display-name` from app-db
```

And now we use that subscription:
```clj
(defn main-panel
  []
  (let [name  (re-frame.core/subscribe [:name])]  ;; <--- a subscription  <---
      [:div "Hello " @name])))   ;; <--- use the result of the subscription
```

The user of our app will see funny things
if that `(subscribe [:name])` doesn't deliver good data. But how do we ensure "good data"?

That will require:
  1. getting data into `app-db`; and
  2. not get into trouble if that data isn't yet in `app-db`.  For example,
  the data may have to come from a server and there's latency.

**Note: `app-db` initially contains `{}`**

### Getting Data Into `app-db`

Only event handlers can change `app-db`. Those are the rules!! Indeed, even initial
values must be put in `app-db` via an event handler.

Here's an event handler for that purpose:
```Clojure
(re-frame.core/reg-event-db
  :initialise-db				 ;; usage: (dispatch [:initialise-db])
  (fn [_ _]						 ;; Ignore both params (db and event)
	 {:display-name "DDATWD"	 ;; return a new value for app-db
	  :items [1 2 3 4]}))
```

You'll notice that this handler does nothing other than to return a ` map`. That map
will become the new value within `app-db`.

We'll need to dispatch an `:initialise-db` event to get it to execute. `main` seems like the natural place:
```Clojure
(defn ^:export main
  []
  (re-frame.core/dispatch [:initialise-db])   ;;  <--- this is new
  (reagent.dom/render [main-panel]
                  (js/document.getElementById "app")))
```

But remember, event handlers execute async. So although there's
a `dispatch` within `main`, the event is simply queued, and the
handler for `:initialise-db`
will not be run until sometime after `main` has finished.

But how long after?  And is there a race condition?  The
component `main-panel` (which assumes good data) might be
rendered before the `:initialise-db` event handler has
put good data into `app-db`.

We don't want any rendering (of `main-panel`) until after `app-db`
has been correctly initialised.

Okay, so that's enough of teasing-out the issues. Let's see a
quick sketch of the entire pattern. It is very straight-forward.

## The Pattern

```Clojure
(re-frame.core/reg-sub   ;; supplied main-panel with data
  :name                  ;; usage (subscribe [:name])
  (fn  [db _]
	(:display-name db)))

(re-frame.core/reg-sub   ;; we can check if there is data
  :initialised?          ;; usage (subscribe [:initialised?])
  (fn  [db _]
	(not (empty? db))))  ;; do we have data

(re-frame.core/reg-event-db
   :initialise-db
   (fn [db _]
       (assoc db :display-name "Jane Doe")))

(defn main-panel    ;; the top level of our app
  []
  (let [name  (re-frame.core/subscribe [:name])]   ;; we need there to be good data
    [:div "Hello " @name])))

(defn top-panel    ;; this is new
  []
  (let [ready?  (re-frame.core/subscribe [:initialised?])]
    (if-not @ready?         ;; do we have good data?
      [:div "Initialising ..."]   ;; tell them we are working on it
      [main-panel])))      ;; all good, render this component

(defn ^:export main     ;; call this to bootstrap your app
  []
  (re-frame.core/dispatch [:initialise-db])
  (reagent.dom/render [top-panel]
                  (js/document.getElementById "app")))
```

## Scales Up

This pattern scales up easily.

For example, imagine a more complicated scenario in which your app
is not fully initialised until 2 backend services supply data.

Your `main` might look like this:
```Clojure
(defn ^:export main     ;; call this to bootstrap your app
  []
  (re-frame.core/dispatch [:initialise-db])           ;; basics
  (re-frame.core/dispatch [:load-from-service-1])     ;; ask for data from service-1
  (re-frame.core/dispatch [:load-from-service-2])     ;; ask for data from service-2
  (reagent.dom/render [top-panel]
                  (js/document.getElementById "app")))
```

Your `:initialised?` test then becomes more like this sketch:

```Clojure
(re-frame.core/reg-sub
  :initialised?          ;; usage (subscribe [:initialised?])
  (fn  [db _]
    (and  (not (empty? db))
          (:service1-answered? db)
          (:service2-answered? db)))))
```

This assumes boolean flags are set in `app-db` when data was loaded from these services.

## Cheating - Synchronous Dispatch

In simple cases, you can simplify matters by using `dispatch-sync` (instead of `dispatch`) in
the main function.

This technique can be seen in the [TodoMVC Example](https://github.com/day8/re-frame/blob/master/examples/todomvc/src/todomvc/core.cljs#L49).

`dispatch` queues an event for later processing, but `dispatch-sync` acts
like a function call and handles an event immediately. That's useful for initial data
load we are considering, particularly for simple apps. Using `dispatch-sync` guarantees
that initial state will be in place before any views are mounted, so we know they'll
subscribe to sensible values.  We don't need a guard like `top-panel` (introduced above).

But don't get into the habit of using `dispatch-sync` everywhere. It is the right
tool in this context and, sometimes, when writing tests, but
`dispatch` is the staple you should use everywhere else.

## Loading Initial Data From Services

Above,  in our example `main`, we imagined using `(re-frame/dispatch [:load-from-service-1])`  to request data
from a backend services.  How would we write the handler for this event?

The next Tutorial will show you how.
````

## File: docs/re-frame.md
````markdown
<p align="center"><img src="../images/logo/re-frame-colour.png?raw=true" alt="The re-frame Logo"></p>

A framework for building Modern Web Apps in ClojureScript. It leverages React, via [Reagent].

McCoy might report "It's MVC, Jim, but not as we know it".  And you would respond
"McCoy, you trouble maker, why even mention an OO pattern?
re-frame is a **functional framework**."

So, it is about `data`, and the `functions`
which transform that data.  And, because it is a **reactive framework**, `data` coordinates
`functions`, not the other way around.

<!--
re-frame's original tag line was ~~derived values, flowing~~ but that's not nearly
pretentious enough for a modern framework. So, instead, it is now
**_putting state into state of the art_**.
Ahhh, yes, I can feel those GitHub stars building already.
-->


[Reagent]:http://reagent-project.github.io/

## Why Should You Care?

Perhaps:

1.  You want to develop a modern web application using ClojureScript.
2.  You want to maximise developer productivity by writing [fewer lines of code](https://medium.com/dailyjs/a-realworld-comparison-of-front-end-frameworks-2020-4e50655fe4c1). You want a simple dynamic process that you can simulate in your head. And you want a clean approach to effects and state management.
2.  You are curious about the benefits of **_data-oriented design_**.
2.  You are a refugee from technical churn, seeking stability and productivity.
    For six years, ClojureScript, Reagent and re-frame have barely changed. No need. Still cutting edge.
2.  You want to see how `reactive programming`, `functional programming` and `immutable data`
    combine in a language that genuinely embraces those paradigms.
3.  You're taking a [Functional Design and Programming course](http://www.eli.sdsu.edu/courses/fall15/cs696/index.html) at San Diego State University
    and you have a re-frame assignment due.  You've left the reading a bit late, right?
4.  You seek a better Redux, Elm, Cycle.js or Pux. In this space, re-frame is very old,
    hopefully in a Gandalf kind of way.
    Designed in late 2014, it slightly pre-dates the official Elm Architecture,
    although thankfully we picked up `foldp` ideas from early Elm games.
    Our main inspiration was the
    Clojure projects [Pedestal App], [Hoplon] and [Om]. Since then,
    re-frame has pioneered ideas like event handler middleware,
    coeffect accretion, and de-duplicated signal graphs.
5.  Which brings us to the most important point: **re-frame is impressively buzzword compliant**. It has reactivity,
    unidirectional data flow, pristinely pure functions,
    interceptors, coeffects, conveyor belts, algebraic effects, statechart-friendliness
    and claims an immaculate hammock conception. All while being both simple and easy. There's also a charming
    xkcd reference (soon) and a hilarious, insiders-joke T-shirt,
    ideal for conferences (in design).

    What could possibly go wrong?

[Pedestal App]:https://github.com/pedestal/pedestal-app
[SPA]:http://en.wikipedia.org/wiki/Single-page_application
[OM]:https://github.com/swannodette/om
[Hoplon]:http://hoplon.io/



## It Is Mature

re-frame was released in early 2015, and has since
[been](https://www.fullcontact.com) successfully
[used](https://www.nubank.com.br) by
[many](http://open.mediaexpress.reuters.com/) a
[companies](https://rokt.com/) and
individuals to build complex apps, many running beyond 40K lines of
ClojureScript.

<img align="right" src="../images/scale-changes-everything.jpg">

**Scale changes everything.** Frameworks
are just pesky overhead at small scale - measure them instead by how they help
you tame the complexity of bigger apps, and in this regard re-frame has
worked out well. Some have been effusive in their praise.

And, yes, re-frame is fast, straight out of the box. And, yes, it has
a good testing story (unit and behavioural). And, yes, it works with
tools like figwheel or shadow-cljs to create
a powerful hot-loading development story. And, yes, it has
fun specialist tooling, and a community,
and useful 3rd party libraries.



<!--
Don't delete the following blank H1, even though it looks useless and a mistake.
It is a trick to stop mkdocs adding a title for this page.
We want the logo to be the title.

Apparently, with mkdocs, if a page has any H1 element in it,
even at the end, like this useless one, a title won't be automatically put at the top.
 -->
#
````

## File: docs/reusable-components.md
````markdown
In re-frame, there are two kinds of Component:

- **Reagent Components** - widgets representing a single value, like a number or choice or string
- **re-frame Components** - larger widget complexes, often representing an entity

## The Essence Of A Component

All Components have:

  - two responsibilities
  - and two associated requirements

The two responsibilities:

1. **to render a value**<br>
   That value could be as simple as a string or as complicated as an entire Pivot Table.
   It may, optionally, also render affordances allowing the user to modify the value. E.g. a spinner supplies up/down buttons.
   Or, for a pivot table, the user can drag "dimension fields" from one place to another to configure the data rollups it displays.

2. **to capture and communicate user intent**<br>
   if the user interacts with the Component to modify the value, then it must communicate the user's intent
   to the surrounding application logic, so it can be interpreted and acted upon.

To fulfil these two responsibilities, Components have **two associated requirements**:

1. a way to **obtain the value** they represent
2. a way to **communicate user intent**

One of these requirements relates to `Input` (obtaining), and the other to `Output` (communicating intent), so we'll
collectively refer to them as the Component's **I/O requirements**.

## Reagent Components

The simplest Components are **Widgets**, which represent a single value like an integer, string or selection.

You can create them from base HTML elements such as `<input>` or `<select>`
using only **Reagent** (no re-frame) and,
for that reason, they are referred to as `Reagent Components`.

Here's an example:
```clj
(defn simple-text-input
  [value callback-fn]
  [:input
   {:type      "text"
    :value     value                                     ;; initial value
    :on-change #(callback-fn (-> % .-target .-value))}]) ;; callback with value
```

You'll notice that both of this Component's **I/O requirements** are provided via two positional arguments:

1. a value (input)
2. a callback function - a means of communicating the user's intent for change (output)

Because both of these requirements are satisfied via arguments, this Component
is quite reusable. We could use it for any string value.

But, of course, the responsibility for providing I/O requirements doesn't disappear. It has just been shifted to the parent Component. This parent will have to act as the glue which "wires" this
reusable Component into an application context, providing the value and actioning user intent.

<!--
When something is reusable, something has to "bind" or ground it into the application. Wire it in.
-->


## re-frame Components

A `re-frame Component` is different from a `Reagent Component`.
because of how it satisfies its **I/O requirements**:

-  it will use `subscribe` to obtain values  (input)
- it will use `dispatch` to communicate events modelling user intent  (output)

re-frame Components tend to be larger. They often represent  **an entire entity**
(not just a single, simple value) and they might involve a "complex of widgets"
with a cohesive purpose.

Here's an example:

```clj
(defn customer-names
  []
  (let [customer @(subscribe [:customer])]              ;; obtain the customer entity
    [:div "Name:"

      ; first name
      [simple-text-input
        (:first-name customer)                          ;; first-name from entity
        #(dispatch [:change-customer :first-name %])]   ;; first name changed

       ;; last name
       [simple-text-input
        (:last-name customer)                           ;; last-name from entity
        #(dispatch [:change-customer :last-name %])]])) ;; last name changed
```

Notes:

- This is a `re-frame Component` because it uses `subscribe` and `dispatch` to provide its I/O requirements
- It composes two other components - Reagent components - the reusable `simple-text-input` we created above
- It parameterises the I/O requirements for the two sub-components by supplying a `value` and a `callback` to each

## Many Instances

But this re-frame Component only works when there is one `Customer` - you'll notice it contained the code:

```clj
(subscribe [:customer])
```

What if our application has many `Customers`? We'd want a Component that can represent
any one of them, or we might need to render many Customers in the UI at once
(not just one at a time).

How then should we rewrite this Component so it can represent Customer entity `A` one time, and
Customer entity `B` another time?  A Component instance representing entity `A` would have to `subscribe`
to the `value` representing `A`. And any events it dispatches must cause changes to
`A`, not `B`.

Method:

- supply each `Component` instance with the **identity** of the Customer entity it should represent
- this `identity` is supplied as an argument  (typically)
- each Component instance will use this `identity` within the query vector given to `subscribe` - so the query is parameterised by the `identity`
- the subscription handler will use this `identity` to obtain the entity's value
- likewise, when events are `dispatched`, they too will include `identity`, so the `event handler` knows which entity to modify

Here's the rewrite which implements this method:
```clj
(defn customer-names
  [id]                                                  ;; customer `id` as argument
  (let [customer @(subscribe [:customer id])]           ;; obtain the value using `id`
    [:div "Name:"

      ; first name
      [simple-text-input
        (:first-name customer)
        #(dispatch [:change-customer id :first-name %])]   ;;  include `id`

       ;; last name
       [simple-text-input
        (:last-name customer)
        #(dispatch [:change-customer id :last-name %])]])) ;; include `id`
```

## What Is Identity?

An `identity` distinguishes one entity from
another - it is something that distinguishes the entity `A` from entity `B`.
In a different technology context, it might be called "a pointer"
(a memory address), "a reference" or "a unique key".

Every entity is stored in `app-db`
and, consequently, one reliable `identity` is the  **path** to that
entity's location within  `app-db`.  Such paths are vectors - paths are data. They are like a pointer to a place within `app-db`.

So, the `identity` for entity `A` could be the path vector
to `A`'s location, for example `[:entities :customers 123]`.  In effect, if you did:
```clj
   (get-in db [:entities :customers 123])
```
you would get the entity. And the `identity` for `B` might be
the same other than for the last element, `[:entities :customers 456]`. In this fictional
scenario, the entities `A` and `B` are both stored in a map the location `[:entities :customers]` within `app-db`,
but you would access them via different keys in that map (`123` vs `456`).

Sometimes, the `identity` need only be the last part of the `path` - the `123` or `456` part
in the example above. The location of the map, `[:entities :customers]`, within `app-db` could be "known" by the
subscription handlers and event handlers, so it doesn't have to be provided, and only the final part of
the path (a key in the map at that location) is needed to distinguish two identities.

So, in summary, an `identity` is usually a path or a path segment.

## Using Identity

Here's how we could use our reusable Component multiple times on one page to show many customers:
```clj
(defn customer-list
  []
  [:div
   (for [id @(subscribe [:all-customer-ids])]
     ^{:key id} [customer-names id])])
```

Notice that `id` is provided for each instance (see the code `[customer-names id]`). That's the entity identity.

## Multiple Identities

Sometimes we need to provide more than one `identity` to a Component.

For example, a dropdown Component might need:

- one `identity` for the list of alternative "choices" available to the user to select
- one `identity` for the current choice (value) held elsewhere within `app-db`

Such a Component would need two arguments (props) for these two `identities`, and it would need
a way to use the identities in subscriptions.

## Computed Identities

`identities` are data, and you can compute data.

When a parent Component has a child sub-component, the parent
might provide its child with an `identity` which is derivative of the `id`
supplied to it.  Perhaps this `identity` is built by `conj`-ing a further
value onto the original `id`.

There are many possibilities.

In another situation, the `id` provided to a component might reference an entity that itself
"contains" the `identity` of a different entity - a reference to a reference.
So, the Component might have to subscribe to the primary entity and then, in a second step,
subscribe to the derived entity.

If we explore these ideas far enough, we leave behind discussions about re-frame and start, instead, to
discuss the pros and cons of the "data model" you have created within `app-db`.

## Components In A Library

Have you noticed the need for close coordination between a re-frame Component
and the subscriptions and dispatches which service it's I/O Requirements?

A re-frame Component doesn't stand by itself - it isn't the unit of reuse.

Because a Component has two `I/O` requirements, the unit of reuse is the re-frame Component
plus the mechanism needed to service those `I/O` requirements. That's what should be
packaged up and put in your library if you want to reuse a Component across multiple applications.

So, just to be clear: the unit of reuse is the combination of:

- the Component
- the subscription handlers which service its need to obtain values
- the event handlers which service the user intent it captures


## Implications

Because a reusable `re-frame Component` has three parts, there is another level of abstraction possible.

Until now, I've said that a `re-frame Component` is defined by its use of `subscribe` and `dispatch`, but maybe it doesn't have to be that way.

Here is a rewrite of that earlier Component:

```Clojure
(defn customer-names
  [id get-customer-fn cust_change-fn]
  (let [customer (get-customer-fn id)]      ;; obtain the value
    [:div "Name:"

      ;; first name
      [simple-text-input
        (:first-name customer)                ;; obtain first-name from the entity
        #(cust_change-fn id :first-name %)]   ;; first name has changed

      ;; last name
      [simple-text-input
        (:last-name customer)                 ;; obtain last-name from the entity
        #(cust_change-fn id :last-name %)]])) ;; last name has changed
```

Notes:

- there's now no sign of `dispatch` or `subscribe`
- instead, the Component is parameterised by two extra function arguments
- these functions handle the I/O requirements

> it is almost as if we have gone full circle now, and we're back to something
> which looks like a Reagent Component.  Remember that one at the very top?
> The I/O requirements were handled via arguments, which shifts "knowledge" about the
> application context to the parent.


Let's rewrite `customer-list` in terms of this new Component:
```Clojure
(defn customer-list
  []
  (let [get-customer (fn [id] @(subscribe [:customer id]))
        put-customer (fn [id field val] (dispatch [:cust-change id field val]))])
  [:div
   (for [id @(subscribe [:all-customer-ids])]
     ^{:key id} [customer-names id get-customer put-customer])])
```
Notes:

- we create `I/O functions` which wrap `subscribe` and `dispatch`
- these two functions are passed into the sub-component as arguments

But does this approach mean the `customer-names` Component is now more reusable? Well, yes, probably.
The exact subscription query to use is now no longer embedded in
the Component itself. The surrounding application supplies that. The Component
has become even more independent of its context. It is even more reusable and flexible.
On the downside, the parent context has more "knowledge" and responsibility. It has to "wire" the Component into the application.

Obviously, there's always a cost to abstraction. So, you'll have to crunch
the cost-benefit analysis for your situation.

## Reusability

There are two levels of reusability:

  - reusability within a particular application.
    You want to use a Component across multiple entities and perhaps in different widget ensembles within the one application.
  - reusability across applications. Put the Component in a library.

`Reagent components` are reusable in both ways - just look at a library like `re-com`.

With `re-frame Components`, reuse is fairly easy
within the one
application, but when you try to put them in a library for use across multiple
applications you run into a challenge to solve: ***placefulness***.

We noted earlier that re-frame Components extend to include the handlers which look after the I/O requirements. And those I/O handlers
have to know **where**, within `app-db` to obtain and update data.

But, from one application to another, the path (the place) where entities are
stored can change. A library Component should not be dictating this "place" to the applications which use it.

## Solving Placefulness

**First**, you could ignore the issue because either:
  1. you have a single app to maintain, and you are optimising for simplicity  (over generality and reusability)
  2. you will avoid using re-frame components, and instead, you just use Reagent Components
     (e.g. we have a rather complicated Table Component which is just a Reagent Component)

**Second**, to solve placefulness, you could standardise where entities are placed within `app-db`. You could
mandate that entities are always stored at a known place (eg. `[:entities :Customers]` or `[:entities :Products]`).
You could then write your reusable components with this assumption, and all your applications adhere to this stipulation.
You could perhaps use [Subgraph](https://github.com/den1k/subgraph).

**Third**, you can parameterise the Component with base-path information via:
  - React context (probably not)
  - via args to the Component - ie. quite literally pass in the base path as an argument
    to the Component and then pass that along to the handlers by including that path in
    the dispatched event and subscription query vectors.

**Fourth**, more radically, you could choose not to use the `map` in `ratom` approach that is `app-db`.
We could use a data structure that is less placeful. Perhaps use a `DataScript` database via [re-posh](https://github.com/denistakeda/re-posh).


**Not doing** - one thing we won't be doing is storing state in the Component itself, away from the
central "data store". The moment we did that, we would have created multiple "stores of state", and then we'd have responsibility for coordinating the sync-ing of those data stores,
a process which starts off looking simple enough but which soon envelops your architecture like an octopus. Managing distributed state is a much more difficult problem than placefulness.


## Summary

Reagent components are readily reusable, and re-frame Components can be made reusable, subject to solving the placefulness issue.
````

## File: docs/start-coding.md
````markdown
## Install Clojure

Install Clojure and Leiningen (a build tool) by following [these instructions](https://purelyfunctional.tv/guide/how-to-install-clojure/).


## Use A Template

Create a scaffold for your first project:

  - Client only:  <https://github.com/day8/re-frame-template>
  - Full Stack: <http://www.luminusweb.net/>


## Full Stack Example

Take time to review a full stack application.  [RealWorld](https://github.com/jacekschae/conduit) implements to the [RealWorld Spec and API](https://github.com/gothinkster/realworld).
````

## File: docs/subscriptions.md
````markdown
# Subscriptions

This tutorial covers dominoes 4, 5 and 6. There are just two API functions to understand - `subscribe` and `reg-sub` - but first let's get an overview.

## On Derived Data

A UI is just derived data.

A browser renders tree-shaped data called the DOM. Reagent renderers create DOM by computing hiccup-shaped data. Subscriptions deliver data to Reagent renderers. And, `app-db` is the root of this entire flow.

When Domino 3 (an effect handler) modifies `app-db`, boom, boom, boom go dominoes 4, 5 & 6,
computing stages of the "materialised view" that is ultimately DOM.  Together these three dominoes implement a reactive dataflow.


## How Exactly?

Data flows through **The Signal Graph**.

`app-db` is the ground truth of a re-frame app, and it is at the root of a
Directed Acyclic Graph (DAG) called the **Signal Graph**.  At the other extent of this
graph - at the leaves - are the `View Functions`, which calculate hiccup.

Typically, the Signal Graph is not deep, with only a few interior layers of nodes
between root and leaves. These interior nodes are the subscription nodes
that you create via `reg-sub`. Or, more accurately, you use `reg-sub` to register
how such interior nodes should be created, if and when they are needed.

Data flows through this graph, being transformed by the interior nodes of its journey and, as a result, the data which
arrives at the leaf `View Functions` will be **a materialised view** of what was originally in `app-db`.

The nodes of the graph are pure functions. When data flows along an input arc and into a node,
it becomes "an argument" (an input) to that node's pure function. That function will be "called" with
the input arguments from input arcs, and it will calculate a return value, more data, which then flows along
that node's output arcs to child nodes, where the process repeats. Ultimately, data is delivered into `View Functions`
via a call to `subscribe`.

It is derived data all the way through the graph. Even the hiccup produced by leaf nodes is
just more derived data. A re-frame app is 75% derived data. I just made that number up,
but you get the idea: there's quite a bit of it.

Hell, the process doesn't even stop with leaf `View Functions`. Hiccup is turned into DOM, which is more derived data.
And the browser turns DOM into pixels on your monitor - yep, more data.
And a monitor turns pixels into photons (data, don't fight me here, I'm on a roll),
which your eye cells detect and turn into chemicals reactions (data) which cause nerve cell signals (totally data),
which reaches the priors in your brain (data). Derived data all the way, baby!  Your brain is domino 12.

Too much? Okay, fine. Just the Signal Graph, then.

## The Four Layers

Conceptually, all nodes in the `Signal Graph` are a part of the same dataflow, but it is
instructive to label them as follows:

   - `Layer 1` - **Ground truth** - is the root node, `app-db`
   - `Layer 2` - **Extractors** - subscriptions which extract data directly from `app-db`, but do no further computation.
   - `Layer 3` - **Materialised View** - subscriptions which obtain data from other subscriptions (never `app-db` directly),
      and compute derived data from their inputs
   - `Layer 4` - **View Functions** - the leaf nodes which compute hiccup (DOM). They `subscribe` to values calculated by Layer 2 or Layer 3 nodes.


The simplest version of the Signal Graph has no `Layer 3` (Materialised View) nodes.
It only has `Layer 2` (Extractor) subscriptions which take data from `app-db`, and those values
then flow unchanged into `Layer 4` (View Functions).

In more complex cases, a `View Function` needs a materialised view
of the data in `app-db`.
A `Layer 2` (extractor) subscription will obtain a data fragment of `app-db`
which will then flow into a `Layer 3` (materialized view) node which will compute
derived data from it and, only then, does data flow into the  `Layer 4` (View Function)


## As Infographic

<img src="../images/subscriptions.png?raw=true">

## Graph Creation

Although data flows through the `Signal Graph` from `app-db` towards the
`View Functions`, graph formation happens in the opposite direction.

When a `View Function` uses a subscription, like this `(subscribe [:something :needed])`,
the sub-graph of nodes needed to service
that subscription is created. The necessary sub-graph will "grow backwards" from the `View Function`
all the way to `app-db`. So it is "the data-thirsty demands" of currently rendered
`View Functions` which dictate what nodes exist in the `Signal Graph`.

And, when a `View Function` is no longer rendered, the sub-graph needed to service
its needs will be destroyed, unless it is still needed to
service the needs of another, current `View Function`.

## Propagation Pruning

The Signal Graph is reactive. When a node's inputs change, the node's subscription handler (function)
re-runs automatically. The value it returns then becomes the node's new output value, and it will
flow to downstream nodes in the graph, causing them to also re-run.

But this only happens if the handler's output is different to the "last time" it ran.
If a handler's return value "this time" is
the same as "last time", data is not propagated to the sub-graph. No need. Nothing has changed.

The computation for each node is performed by a pure function and a pure function will return
the same value each time it is called with the same arguments. So, if we were to give a downstream node
the same inputs as last time, it would produce the same outputs as last time, including the same hiccup at the leaves.

## Different How?

Data values "this time" and "last time" are regarded as "being the same" if ClojureScript's `=` says they are.


## Why Layer 2 - Extractors?

Why is a layer of "extractors" necessary?

**It is an efficiency thing.** `app-db` will be changed by almost every `event`, often in a small,
partial way. But any change whatsoever will cause all `Layer2` (extractor) subscription to be automatically re-run.
All of them. Every time. This is because `app-db` is their input value, and subscriptions re-run when
one of their inputs change.

Extractors obtain a data fragment from `app-db` and then immediately prune
further propagation of that  value if the fragment is the same "last time". As a consequence,
the CPU intensive work in the `Layer 3` (materialised view) and `Layer 4` (View Functions) is only performed when necessary.

`Layer 2` (extractors) act as the Signal Graph's circuit breakers. We want them to be as computationally simple as possible.

## reg-sub

Subscription handlers are registered using `reg-sub`. These handlers are the functions which take
input values, flowing into the node, and calculate a derived value to be the node's output.


Extractor subscriptions are registered like this:
```clj
(re-frame.core/reg-sub  ;; a part of the re-frame API
  :id                   ;; usage: (subscribe [:id])
  (fn [db query-v]      ;; `db` is the map out of `app-db`
    (:something db)))   ;; trivial extraction - no computation
```

This registers a `computation function` - a pretty simple one which just does an extraction. The argument `query-v`
is the `query vector` supplied in the subscription. In our simple case here, we're not using it. But if the subscription was for
`(subscribe [:id "blue" :yeah])` then the `query-v` given to the handler would be `[:id "blue" :yeah]`.

`Layer 3` (materialised view) subscriptions depend on other subscriptions for their inputs, and they are registered like this:
```clj
(reg-sub
  :id

  ;; signals function
  (fn [query-v]
    [(subscribe [:a]) (subscribe [:b 2])])     ;; <-- these inputs are provided to the computation function

  ;; computation function
  (fn [[a b] query-v]                  ;; input values supplied in a vector
      (calculate-it a b)))
```
You supply two functions:

1. a `signals function` which returns the input signals for this kind of node. It
   can return either a single signal, or a vector of signals, or a map where the
   values are the signals. In the example above, it is returning a 2-vector of signals.

2. a `computation function` which takes
  the input values provided by the `signals function`, supplied as the first argument,
  and it produces a new derived value which will be the output of the node.


!!! Note "Registration Doesn't Mean A Node Exists"
    When you use `reg-sub` to register a handler, you are not immediately
    creating a node in the Signal Graph.
    At any one time, only those nodes required to service the needs of **current** `View Functions` will exist.
    Registering a handler only says how to create a Signal Graph node when and if it is needed.

## Syntactic Sugar

The `Layer 3` (materialized view) subscription above can be rewritten using some syntactic sugar:
```clj
(reg-sub
  :id

  ;; input signals
  :<- [:a]        ;; means (subscribe [:a] is an input)
  :<- [:b 2]      ;; means (subscribe [:b 2] is an input)

  ;; computation function
  (fn [[a b] query-v]
       (calculate-it a b)))
```
````

## File: docs/Talking-To-Servers.md
````markdown
## Talking To Servers

This page describes how a re-frame app might "talk" to a backend HTTP server.

We'll assume there's a json-returning server endpoint
at "http://json.my-endpoint.com/blah". We want to GET from that
endpoint and put a processed version of the returned json into `app-db`.

## Triggering The Request

The user often does something to trigger the process.

Here's a button which the user could click:
```clj
(defn request-it-button
  []
  [:div {:class "button-class"
         :on-click  #(dispatch [:request-it])}  ;; get data from the server !!
         "I want it, now!"])
```

Notice the `on-click` handler - it `dispatch`es the event `[:request-it]`.

## The Event Handler

That `:request-it` event will need to be "handled", which means an event handler must be registered for it.

We want this handler to:
  1. Initiate the HTTP GET
  2. Update a flag in `app-db` which will trigger a modal "Loading ..." message for the user to see

We're going to create two versions of this event handler.  First, we'll create a
problematic version of the event handler and then, realising our sins, we'll write
a second version which is a soaring paragon of virtue. Both versions
will teach us something.


### Version 1

We're going to use the [cljs-ajax library](https://github.com/JulianBirch/cljs-ajax) as the HTTP workhorse.

Here's the event handler:
```clj
(ns my.app.events                   ;; <1>
   (:require [ajax.core :refer [GET]]
             [re-frame.core :refer [reg-event-db]))

(reg-event-db        ;; <-- register an event handler
  :request-it        ;; <-- the event id
  (fn                ;; <-- the handler function
    [db _]

    ;; kick off the GET, making sure to supply a callback for success and failure
    (GET
      "http://json.my-endpoint.com/blah"
      {:handler       #(dispatch [:process-response %1])   ;; <2> further dispatch !!
       :error-handler #(dispatch [:bad-response %1])})     ;; <2> further dispatch !!

     ;; update a flag in `app-db` ... presumably to cause a "Loading..." UI
     (assoc db :loading? true)))    ;; <3> return an updated db
```

Further Notes:
  1. Event handlers are normally put into an `events.cljs` namespace
  2. Notice that the GET callbacks issue a further `dispatch`. Such callbacks
   should never attempt to close over `db` themselves, or make
   any changes to it because, by the time these callbacks happen, the value
   in `app-db` may have changed.  Whereas, if they `dispatch`, then the event
   handlers looking after the event they dispatch will be given the latest copy of the db.
  3. event handlers registered using `reg-event-db` must return a new value for
   `app-db`.  In our case, we set a flag which will presumably cause a "Loading ..."
   UI to show.

### Successful GET

As we noted above, the on-success handler itself is just
`#(dispatch [:process-response RESPONSE])`.  So we'll need to register a handler
for this event too.

Like this:
```clj
(reg-event-db
  :process-response
  (fn
    [db [_ response]]           ;; destructure the response from the event vector
    (-> db
        (assoc :loading? false) ;; take away that "Loading ..." UI
        (assoc :data (js->clj response))))  ;; fairly lame processing
```

A normal handler would have more complex processing of the response. But we're
just sketching here, so we've left it easy.

There'd also need to be a handler for the `:bad-response` event too.  Left as an exercise.

### Problems In Paradise?

This approach will work, and it is useful to take time to understand why it
would work, but it has a problem:  the event handler isn't pure.

That `GET` is a side effect, and side effecting functions are like a
well salted paper cut. We try hard to avoid them.

### Version 2

The better solution is, of course, to use an effectful handler. This
is explained in detail in the previous tutorials: [Effectful Handlers](EffectfulHandlers.md)
and [Effects](Effects.md).

In the 2nd version, we use the alternative registration function, `reg-event-fx`, and we'll use an
"Effect Handler" supplied by this library
[https://github.com/day8/re-frame-http-fx](https://github.com/day8/re-frame-http-fx).
You may soon feel confident enough to write your own.

Here's our rewrite:

```clj
(ns my.app.events
   (:require
      [ajax.core :as ajax]
      [day8.re-frame.http-fx]
      [re-frame.core :refer [reg-event-fx]))

(reg-event-fx        ;; <-- note the `-fx` extension
  :request-it        ;; <-- the event id
  (fn                ;; <-- the handler function
    [{db :db} _]     ;; <-- 1st argument is coeffect, from which we extract db

    ;; we return a map of (side) effects
    {:http-xhrio {:method          :get
                  :uri             "http://json.my-endpoint.com/blah"
                  :format          (ajax/json-request-format)
                  :response-format (ajax/json-response-format {:keywords? true})
                  :on-success      [:process-response]
                  :on-failure      [:bad-response]}
     :db  (assoc db :loading? true)}))
```

Notes:
  1. Our event handler "describes" side effects, it does not "do" side effects
  2. The event handler we wrote for `:process-response` stays as it was
````

## File: docs/Testing.md
````markdown
# Testing

This is an introduction to testing re-frame apps. It
walks you through some choices.

## What To Test

For any re-frame app, there's three things to test:

  - **Event Handlers** - most of your testing focus will
     be here because this is where most of the logic lives

  - **Subscription Handlers** - often not a lot to test here. Only
    [Layer 3](subscriptions.md) subscriptions need testing.

  - **View functions** - I don't tend to write tests for views. There, I said it.
    Hey!  It is mean to look at someone with that level of disapproval,
    while shaking your head. I have my reasons ...<br>
    In my experience with the re-frame architecture, View Functions
    tend to be an unlikely source of bugs. And every line of code you write is
    like a ball & chain you must forevermore drag about, so I dislike maintaining
    tests which don't deliver good bang for buck.

And, yes, in theory there's also `Effect Handlers` (Domino 3) to test,
but you'll hardly ever write one, and, anyway, each one is different, so
I've got no good general insight to offer you for them. They will be ignored
in this tutorial.

## Test Terminology

Let's establish some terminology to aid the further explanations in this
tutorial.  Every unittest has 3 steps:

  1. **setup** initial conditions
  2. **execute** the thing-under-test
  3. **verify** that the thing-under-test did the right thing

## Exposing Event Handlers For Test

Event Handlers are pure functions which should make them easy to test, right?

First, create a named event handler using `defn` like this:

```clj
(defn select-triangle
  [db [_ triangle-id]]
  ... return a modified version of db)
```

You'd register this handler in a separate step:

```clj
(re-frame.core/reg-event-db     ;; this is a "-db" event handler, not "-fx"
  :select-triangle
  [some-interceptors]
  select-triangle)    ;; <--- defn above. don't use an annonomous fn
```

This arrangement means the event handler function
`select-triangle` is readily available to be unittested.

## Event Handlers - Setup - Part 1

To test `select-triangle`, a unittest must pass in values for the two arguments
`db` and `v`. And, so, our **setup** would have to construct both values.

But how to create a useful `db` value?

`db` is a map of a certain structure, so one way would be to simply `assoc` values
into a map at certain paths to simulate a real-world `db` value or, even easier, just use
a map literal, like this:

```clj
;; a test
(let [
      ;; setup - create db and event
      db      {:some 42  :thing "hello"}   ; a literal
      event   [:select-triangle :other :event :args]

      ;; execute
      result-db (select-triange db event)]

      ;; validate that result-db is correct)
      (is ...)
```

This certainly works in theory, but in practice,
unless we are careful, constructing the `db`
value in **setup** could:

  * be manual and time-consuming
  * tie tests to the internal structure of `app-db`

The **setup** of every test could end up relying on the internal structure
of `app-db` and any change in that structure (which is inevitable over time)
would result in a lot re-work in the tests. That's too fragile.

So, this approach doesn't quite work.

## Event Handlers - Setup - Part 2

> In re-frame, `Events` are central. They are the "language of the system". They
provide the eloquence.


The `db` value (stored in `app-db`) is the cumulative result
of many event handlers running.

We can use this idea.  In **setup**, instead of manually trying to create that `db` value, we could
"build up" a `db` value by threading `db` through many event handlers
which cumulatively create the required initial state.  Tests then need
know nothing about the internal structure of that `db`.

Like this:

```clj
(let [
      ;; setup - cummulatively build up db
      db (-> {}    ;; empty db
             (initialise-db [:initialise-db])   ;; each event handler expects db and event
             (clear-panel   [:clear-panel])
             (draw-triangle [:draw-triangle 1 2 3]))

      event  [:select-triange :other :stuff]

      ;; now execute the event handler under test
      db'    (select-triange db event)]

      ;; validate that db' is correct
      (is ...)

```

This approach works so long as all the event handlers are
of the `-db` kind, but the threading gets a little messy when some event
handlers are of the `-fx` kind which take a `coeffect` argument and
return `effects`, instead of a `db` value.

So, this approach is quite workable in some cases, but can get messy
in the general case.

## Event Handlers - Setup - Part 3

There is further variation which is quite general but not as pure.

During test **setup** we could literally just `dispatch` the events
which would put `app-db` into the right state.

Except, we'd have to use `dispatch-sync` rather `dispatch` to
force immediate handling of events, rather than queuing.

```clj
;; setup - cummulatively build up db
(dispatch-sync [:initialise-db])
(dispatch-sync [:clear-panel])
(dispatch-sync [:draw-triangle 1 2 3])

;; execute
(dispatch-sync  [:select-triange :other :stuff])

;; validate that the value in 'app-db' is correct
;; perhaps with subscriptions
```

Notes:

  1. we use `dispatch-sync` because `dispatch` is async (event is handled not now, but soon)
  2. Not pure. We are choosing to mutate the global `app-db`. But
     having said that, there's something about this approach which is remarkably
     pragmatic.
  2. the **setup** is now very natural. The associated handlers can be either `-db` or `-fx`
  3. if the handlers have effects other than just updating app-db, we might need to stub out XXX
  4. How do we look at the results ????

If this method appeals to you, you should ABSOLUTELY review the utilities in this helper library:
[re-frame-test](https://github.com/day8/re-frame-test).

In summary, event handlers should be easy to test because they are pure functions. The interesting
part is the unittest "setup" where we need to establish an initial value for `db`.

## Subscription Handlers

Here's a Subscription Handler from
[the todomvc example](https://github.com/day8/re-frame/blob/master/examples/todomvc/src/todomvc/subs.cljs):

```clj
(reg-sub
  :visible-todos

  ;; signal function
  (fn [query-v _]
    [(subscribe [:todos])
     (subscribe [:showing])])

  ;; computation function
  (fn [[todos showing] _]   ;; that 1st parameter is a 2-vector of values
    (let [filter-fn (case showing
                      :active (complement :done)
                      :done   :done
                      :all    identity)]
      (filter filter-fn todos))))
```

How do we test this?

First, we could split the computation function from its registration, like this:
```clj
(defn visible-todos
  [[todos showing] _]

  (let [filter-fn (case showing
                    :active (complement :done)
                    :done   :done
                    :all    identity)]
   (filter filter-fn todos)))

(reg-sub
  :visible-todos
  (fn [query-v _]
      [(subscribe [:todos])
       (subscribe [:showing])])
  visible-todos)     ;; <--- computation function used here
```

That makes `visible-todos` available for direct unit testing.  But, as we experienced
with Event Handlers, the challenge is around constructing `db` values (first parameter)
in a way which doesn't become fragile.

## View Functions - Part 1

Components/views are more tricky and there are a few options.

But remember my ugly secret - I don't tend to write tests for my views.

But here's how, theoretically, I'd write tests if I wasn't me ...

If a View Function is [Form-1](https://github.com/reagent-project/reagent/blob/master/doc/CreatingReagentComponents.md#form-1-a-simple-function),
then it is fairly easy to test.

A trivial example:
```clj
(defn greet
   [name]
   [:div "Hello " name])

(greet "Wiki")
;;=> [:div "Hello " "Wiki"]
```

So, here, testing involves passing values into the function and checking the data structure returned
for correctness.

What's returned is hiccup, of course. So how do you test hiccup for correctness?

hiccup is just a clojure data structure - vectors containing keywords, and maps, and other vectors, etc.
Perhaps you'd use https://github.com/nathanmarz/specter to declaratively check on the presence
of certain values and structures? Or do it more manually.

## View Functions - Part 2A

But what if the View Function has a subscription?

```clj
(defn my-view
  [something]
  (let [val  (subscribe [:query-id])]     ;; <-- reactive subscription
    [:div .... using @val in here]))
```

The use of `subscribe` makes the function impure (it obtains data from places other than its args).

A testing plan might be:

  1. setup `app-db` with some values in the right places  (via dispatch of events?)
  2. call `my-view` (with a parameter) which will return hiccup
  3. check the hiccup structure for correctness.

Continuing on, in a second phase you could then:

  5. change the value in `app-db`  (which will cause the subscription to fire)
  6. call view functions again (hiccup returned)
  7. check the new hiccup for correctness.

Which is all possible, if a little messy.

## View Functions - Part 2B

There is a pragmatic method available to handle the impurity: use `with-redefs`
to stub out `subscribe`. Like this:

```clj
(defn subscription-stub [x]
  (atom
    (case x
      [:query-id] 42)))

(deftest some-test
  (with-redefs [re-frame/subscribe (subscription-stub)]
    (testing "some some view which does a subscribe"
      ..... call the view function and the hiccup output)))
```

For more integration level testing, you can use `with-mounted-component`
from the [reagent-template](https://github.com/reagent-project/reagent-template/blob/master/resources/leiningen/new/reagent/test/cljs/reagent/core_test.cljs)
to render the component in the browser and validate the generated DOM.

## View Functions - Part 2C

Or ... there is another option: you can structure in the first place for pure view functions.

The trick here is to create an outer and inner component. The outer sources the data
(via a subscription), and passes it onto the inner as props (parameters).

As a result, the inner component, which does the testable work, is pure and
easily tested. The outer is impure but trivial.

To get a more concrete idea, I'll direct you to another page in the re-frame docs
which has nothing to do with testing, but it does use this `simple-outer-subscribe-with-complicated-inner-render`
pattern for a different purpose:
[Using Stateful JS Components](Using-Stateful-JS-Components.md)

Note: this technique could be made simple and almost invisible via the
use of macros.

This pattern has been independently discovered by many. For example, here
it is called the [Container/Component pattern](https://medium.com/@learnreact/container-components-c0e67432e005#.mb0hzgm3l).

## Also Read This

[https://juxt.pro/blog/posts/cljs-apps.html](https://juxt.pro/blog/posts/cljs-apps.html)

## Summary

Event handlers will be your primary focus when testing. Remember to review the utilities in
[re-frame-test](https://github.com/day8/re-frame-test).
````

## File: examples/flow/src/re_frame/flow/demo.cljs
````clojure
(ns re-frame.flow.demo
  (:require
   [reagent.dom.client :as rdc]
   [re-frame.alpha :as rf]
   [zprint.core :as zp]
   [clojure.string :as str]
   [re-frame.db :refer [app-db]]))

(defn debug-app-db []
  (fn []
    [:pre {:style {:position "absolute" :bottom 0 :right 0 :font-size 8}}
     (some-> app-db
             deref
             (zp/zprint-str {:style :justified})
             (str/replace #"re-frame.flow.demo/" ":")
             (str/replace #"re-fine." ":"))]))

(rf/reg-sub ::items :-> (comp reverse ::items))

(defn item [id]
  [:div "Item " id])

(defn items []
  (into [:div]
        (map item)
        @(rf/subscribe [::items])))

(rf/reg-event-db
 ::clear-all
 (fn [db _] (dissoc db ::items)))

(rf/reg-event-db
 ::add-item
 (fn [db [_ id]] (update db ::items conj id)))

(rf/reg-event-db
 ::delete-item
 (fn [db [_ id]] (update db ::items #(remove #{id} %))))

(defn controls []
  (let [id (atom 0)]
    (fn []
      [:div
       [:button {:on-click #(do (rf/dispatch [::add-item (inc @id)])
                                (swap! id inc))} "Add"] " "
       [:button {:on-click #(do (rf/dispatch [::delete-item @id])
                                (swap! id dec))} "Delete"] " "
       [:button {:on-click #(do (rf/dispatch [::clear-all])
                                (reset! id 0))} "Clear"] " "])))

(def error-state-flow
  {:id ::error-state
   :path [::error-state]
   :inputs {:items [::items]}
   :output (fn [_ {:keys [items]}]
             (cond
               (> (count items) 2) :too-many
               (empty? items)      :none))
   :live-inputs {:items [::items]}
   :live? (fn [{:keys [items]}]
            (let [ct (count items)]
              (or (zero? ct) (> ct 3))))})

(rf/reg-flow error-state-flow)

(rf/reg-event-fx
 ::clear-flow
 (fn [_ _] {:fx [[:clear-flow ::error-state]]}))

(rf/reg-event-fx
 ::reg-flow
 (fn [_ _] {:fx [[:reg-flow error-state-flow]]}))

(defn flow-controls []
  [:div [:button {:on-click #(do (rf/dispatch [::clear-flow]))}
         "Clear flow"] " "
   [:button {:on-click #(do (rf/dispatch [::reg-flow]))}
    "Register flow"]])

(defn warning []
  (let [error-state (rf/subscribe [:flow {:id ::error-state}])]
    [:div {:style {:color "red"}}
     (->> @error-state
          (get {:too-many "Warning: only the first 3 items will be used."
                :none     "No items. Please add one."}))]))

(defn root []
  [:div [controls] [flow-controls] [warning] [items] [debug-app-db]])

(rf/reg-event-db
 ::init
 (fn [db _] db))

(defonce root-container
  (rdc/create-root (js/document.getElementById "app")))

(defn run
  []
  (rf/dispatch-sync [::init])
  (rdc/render root-container [root]))
````

## File: examples/simple/src/simple/core.cljs
````clojure
(ns simple.core
  (:require [reagent.dom.client :as rdc]
            [re-frame.core :as rf]))

;; A detailed walk-through of this source code is provided in the docs:
;; https://day8.github.io/re-frame/dominoes-live/

;; -- Domino 1 - Event Dispatch -----------------------------------------------

(defn dispatch-timer-event
  []
  (let [now (js/Date.)]
    (rf/dispatch [:timer now])))  ;; <-- dispatch used

;; Call the dispatching function every second.
;; `defonce` is like `def` but it ensures only one instance is ever
;; created in the face of figwheel hot-reloading of this file.
(defonce do-timer (js/setInterval dispatch-timer-event 1000))

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db              ;; sets up initial application state
 :initialize                 ;; usage:  (dispatch [:initialize])
 (fn [_ _]                   ;; the two parameters are not important here, so use _
   {:time (js/Date.)         ;; What it returns becomes the new application state
    :time-color "orange"}))  ;; so the application state will initially be a map with two keys

(rf/reg-event-db                ;; usage:  (dispatch [:time-color-change 34562])
 :time-color-change            ;; dispatched when the user enters a new colour into the UI text field
 (fn [db [_ new-color-value]]  ;; -db event handlers given 2 parameters:  current application state and event (a vector)
   (assoc db :time-color new-color-value)))   ;; compute and return the new application state

(rf/reg-event-db                 ;; usage:  (dispatch [:timer a-js-Date])
 :timer                         ;; every second an event of this kind will be dispatched
 (fn [db [_ new-time]]          ;; note how the 2nd parameter is destructured to obtain the data value
   (assoc db :time new-time)))  ;; compute and return the new application state

;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
 :time
 (fn [db _]     ;; db is current app state. 2nd unused param is query vector
   (:time db))) ;; return a query computation over the application state

(rf/reg-sub
 :time-color
 (fn [db _]
   (:time-color db)))

;; -- Domino 5 - View Functions ----------------------------------------------

(defn clock
  []
  (let [colour @(rf/subscribe [:time-color])
        time   (-> @(rf/subscribe [:time])
                   .toTimeString
                   (clojure.string/split " ")
                   first)]
    [:div.example-clock {:style {:color colour}} time]))

(defn color-input
  []
  (let [gettext (fn [e] (-> e .-target .-value))
        emit    (fn [e] (rf/dispatch [:time-color-change (gettext e)]))]
    [:div.color-input
     "Display color: "
     [:input {:type "text"
              :style {:border "1px solid #CCC"}
              :value @(rf/subscribe [:time-color])        ;; subscribe
              :on-change emit}]]))  ;; <---

(defn ui
  []
  [:div
   [:h1 "The time is now:"]
   [clock]
   [color-input]])

;; -- Entry Point -------------------------------------------------------------

(defonce root-container
  (rdc/create-root (js/document.getElementById "app")))

(defn mount-ui
  []
  (rdc/render root-container [ui]))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (rf/clear-subscription-cache!)
  (mount-ui))

(defn run               ;; Your app calls this when it starts. See shadow-cljs.edn :init-fn.
  []
  (rf/dispatch-sync [:initialize]) ;; put a value into application state
  (mount-ui))                      ;; mount the application's ui into '<div id="app" />'
````

## File: examples/todomvc/src/todomvc/core.cljs
````clojure
(ns todomvc.core
  (:require-macros [secretary.core :refer [defroute]])
  (:require [goog.events :as events]
            [reagent.dom.client :as rdc]
            [re-frame.alpha :as rf :refer [dispatch dispatch-sync]]
            [secretary.core :as secretary]
            [todomvc.events] ;; These two are only required to make the compiler
            [todomvc.subs]   ;; load them (see docs/App-Structure.md)
            [todomvc.views])
  (:import [goog History]
           [goog.history EventType]))

;; -- Debugging aids ----------------------------------------------------------
(enable-console-print!)   ;; so that println writes to `console.log`

;; Put an initial value into app-db.
;; The event handler for `:initialise-db` can be found in `events.cljs`
;; Using the sync version of dispatch means that value is in
;; place before we go onto the next step.
(dispatch-sync [:initialise-db])

;; -- Routes and History ------------------------------------------------------
;; Although we use the secretary library below, that's mostly a historical
;; accident. You might also consider using:
;;   - https://github.com/DomKM/silk
;;   - https://github.com/juxt/bidi
;; We don't have a strong opinion.
;;
(defroute "/" [] (dispatch [:set-showing :all]))
(defroute "/:filter" [filter] (dispatch [:set-showing (keyword filter)]))

(defonce history
  (doto (History.)
    (events/listen EventType.NAVIGATE
                   (fn [^js/goog.History.Event event] (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -- Entry Point -------------------------------------------------------------

(defonce root-container
  (rdc/create-root (.getElementById js/document "app")))

(defn render
  []
  ;; Render the UI into the HTML's <div id="app" /> element
  ;; The view function `todomvc.views/todo-app` is the
  ;; root view for the entire UI.
  (rdc/render root-container [todomvc.views/todo-app]))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (rf/clear-subscription-cache!)
  (render))

(defn ^:export main
  []
  (render))
````

## File: examples/todomvc/src/todomvc/db.cljs
````clojure
(ns todomvc.db
  (:require [cljs.reader]
            [cljs.spec.alpha :as s]
            [re-frame.alpha :as re-frame]))

;; -- Spec --------------------------------------------------------------------
;;
;; This is a clojure.spec specification for the value in app-db. It is like a
;; Schema. See: http://clojure.org/guides/spec
;;
;; The value in app-db should always match this spec. Only event handlers
;; can change the value in app-db so, after each event handler
;; has run, we re-check app-db for correctness (compliance with the Schema).
;;
;; How is this done? Look in events.cljs and you'll notice that all handlers
;; have an "after" interceptor which does the spec re-check.
;;
;; None of this is strictly necessary. It could be omitted. But we find it
;; good practice.

(s/def ::id int?)
(s/def ::title string?)
(s/def ::done boolean?)
(s/def ::todo (s/keys :req-un [::id ::title ::done]))
(s/def ::todos (s/and                                       ;; should use the :kind kw to s/map-of (not supported yet)
                (s/map-of ::id ::todo)                      ;; in this map, each todo is keyed by its :id
                #(instance? PersistentTreeMap %)            ;; is a sorted-map (not just a map)
                ))
(s/def ::showing                                            ;; what todos are shown to the user?
  #{:all                                                    ;; all todos are shown
    :active                                                 ;; only todos whose :done is false
    :done                                                   ;; only todos whose :done is true
    })
(s/def ::db (s/keys :req-un [::todos ::showing]))

;; -- Default app-db Value  ---------------------------------------------------
;;
;; When the application first starts, this will be the value put in app-db
;; Unless, of course, there are todos in the LocalStore (see further below)
;; Look in:
;;   1.  `core.cljs` for  "(dispatch-sync [:initialise-db])"
;;   2.  `events.cljs` for the registration of :initialise-db handler
;;

(def default-db           ;; what gets put into app-db by default.
  {:todos   (sorted-map)  ;; an empty list of todos. Use the (int) :id as the key
   :showing :all})        ;; show all todos

;; -- Local Storage  ----------------------------------------------------------
;;
;; Part of the todomvc challenge is to store todos in LocalStorage, and
;; on app startup, reload the todos from when the program was last run.
;; But the challenge stipulates to NOT load the setting for the "showing"
;; filter. Just the todos.
;;

(def ls-key "todos-reframe")                         ;; localstore key

(defn todos->local-store
  "Puts todos into localStorage"
  [todos]
  (.setItem js/localStorage ls-key (str todos)))     ;; sorted-map written as an EDN map

;; -- cofx Registrations  -----------------------------------------------------

;; Use `reg-cofx` to register a "coeffect handler" which will inject the todos
;; stored in localstore.
;;
;; To see it used, look in `events.cljs` at the event handler for `:initialise-db`.
;; That event handler has the interceptor `(inject-cofx :local-store-todos)`
;; The function registered below will be used to fulfill that request.
;;
;; We must supply a `sorted-map` but in LocalStore it is stored as a `map`.
;;
(re-frame/reg-cofx
 :local-store-todos
 (fn [cofx _]
      ;; put the localstore todos into the coeffect under :local-store-todos
   (assoc cofx :local-store-todos
             ;; read in todos from localstore, and process into a sorted map
          (into (sorted-map)
                (some->> (.getItem js/localStorage ls-key)
                         (cljs.reader/read-string)    ;; EDN map -> map
                         )))))
````

## File: examples/todomvc/src/todomvc/events.cljs
````clojure
(ns todomvc.events
  (:require
   [todomvc.db    :refer [default-db todos->local-store]]
   [re-frame.alpha :refer [reg-event-db reg-event-fx inject-cofx path after sub]]
   [cljs.spec.alpha :as s]))

;; -- Interceptors --------------------------------------------------------------
;;
;; Interceptors are a more advanced topic. So, we're plunging into the deep
;; end here.
;;
;; There is a tutorial on Interceptors in re-frame's `/docs`, but to get
;; you going fast, here's a very high level description ...
;;
;; Every event handler can be "wrapped" in a chain of interceptors. A
;; "chain of interceptors" is actually just a "vector of interceptors". Each
;; of these interceptors can have a `:before` function and an `:after` function.
;; Each interceptor wraps around the "handler", so that its `:before`
;; is called before the event handler runs, and its `:after` runs after
;; the event handler has run.
;;
;; Interceptors with a `:before` action, can be used to "inject" values
;; into what will become the `coeffects` parameter of an event handler.
;; That's a way of giving an event handler access to certain resources,
;; like values in LocalStore.
;;
;; Interceptors with an `:after` action, can, among other things,
;; process the effects produced by the event handler. One could
;; check if the new value for `app-db` correctly matches a Spec.
;;

;; -- First Interceptor ------------------------------------------------------
;;
;; Event handlers change state, that's their job. But what happens if there's
;; a bug in the event handler and it corrupts application state in some subtle way?
;; Next, we create an interceptor called `check-spec-interceptor`.
;; Later, we use this interceptor in the interceptor chain of all event handlers.
;; When included in the interceptor chain of an event handler, this interceptor
;; runs `check-and-throw` `after` the event handler has finished, checking
;; the value for `app-db` against a spec.
;; If the event handler corrupted the value for `app-db` an exception will be
;; thrown. This helps us detect event handler bugs early.
;; Because all state is held in `app-db`, we are effectively validating the
;; ENTIRE state of the application after each event handler runs.  All of it.

(defn check-and-throw
  "Throws an exception if `db` doesn't match the Spec `a-spec`."
  [a-spec db]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "spec check failed: " (s/explain-str a-spec db)) {}))))

;; now we create an interceptor using `after`
(def check-spec-interceptor (after (partial check-and-throw :todomvc.db/db)))

;; -- Second Interceptor -----------------------------------------------------
;;
;; Part of the TodoMVC challenge is to store todos in local storage.
;; Next, we define an interceptor to help with this challenge.
;; This interceptor runs `after` an event handler, and it stores the
;; current todos into local storage.
;; Later, we include this interceptor into the interceptor chain
;; of all event handlers which modify todos.  In this way, we ensure that
;; every change to todos is written to local storage.
(def ->local-store (after todos->local-store))

;; -- Interceptor Chain ------------------------------------------------------
;;
;; Each event handler can have its own chain of interceptors.
;; We now create the interceptor chain shared by all event handlers
;; which manipulate todos.
;; A chain of interceptors is a vector of interceptors.
;; Explanation of the `path` Interceptor is given further below.
(def todo-interceptors [check-spec-interceptor    ;; ensure the spec is still valid  (after)
                        (path :todos)             ;; the 1st param given to handler will be the value from this path within db
                        ->local-store])            ;; write todos to localstore  (after)

;; -- Helpers -----------------------------------------------------------------

(defn allocate-next-id
  "Returns the next todo id.
  Assumes todos are sorted.
  Returns one more than the current largest id."
  [todos]
  ((fnil inc 0) (last (keys todos))))

;; -- Event Handlers ----------------------------------------------------------

;; usage:  (dispatch [:initialise-db])
;;
;; This event is dispatched in the app's `main` (core.cljs).
;; It establishes initial application state in `app-db`.
;; That means merging:
;;   1. Any todos stored in LocalStore (from the last session of this app)
;;   2. Default initial values
;;
;; Advanced topic:  we inject the todos currently stored in LocalStore
;; into the first, coeffect parameter via use of the interceptor
;;    `(inject-cofx :local-store-todos)`
;;
;; To fully understand this advanced topic, you'll have to read the tutorials
;; and look at the bottom of `db.cljs` for the `:local-store-todos` cofx
;; registration.
(reg-event-fx                 ;; part of the re-frame API
 :initialise-db              ;; event id being handled

  ;; the interceptor chain (a vector of 2 interceptors in this case)
 [(inject-cofx :local-store-todos) ;; gets todos from localstore, and puts value into coeffects arg
  check-spec-interceptor]          ;; after event handler runs, check app-db for correctness. Does it still match Spec?

  ;; the event handler (function) being registered
 (fn [{:keys [db local-store-todos]} _]                  ;; take 2 values from coeffects. Ignore event vector itself.
   {:db (assoc default-db :todos local-store-todos)}))   ;; all hail the new state to be put in app-db

;; usage:  (dispatch [:set-showing  :active])
;; This event is dispatched when the user clicks on one of the 3
;; filter buttons at the bottom of the display.
(reg-event-db      ;; part of the re-frame API
 :set-showing     ;; event-id

  ;; only one interceptor
 [check-spec-interceptor]       ;; after event handler runs, check app-db for correctness. Does it still match Spec?

  ;; handler
 (fn [db [_ new-filter-kw]]     ;; new-filter-kw is one of :all, :active or :done
   (assoc db :showing new-filter-kw)))

;; NOTE: below is a rewrite of the event handler (above) using a `path` Interceptor
;; You'll find it illuminating to compare this rewrite with the original.
;;
;; A `path` interceptor has BOTH a before and after action.
;; When you create one, you supply "a path" into `app-db`, like:
;; [:a :b 1]
;; The job of "before" is to replace the app-db with the value
;; of `app-db` at the nominated path. And, then, "after" to
;; take the event handler returned value and place it back into
;; app-db at the nominated path.  So the event handler works
;; with a particular, narrower path within app-db, not all of it.
;;
;; So, `path` operates a little like `update-in`
;;
#_(reg-event-db
   :set-showing

  ;; this now a chain of 2 interceptors. Note use of `path`
   [check-spec-interceptor (path :showing)]

  ;; The event handler
  ;; Because of the `path` interceptor above, the 1st parameter to
  ;; the handler below won't be the entire 'db', and instead will
  ;; be the value at the path `[:showing]` within db.
  ;; Equally the value returned will be the new value for that path
  ;; within app-db.
   (fn [old-showing-value [_ new-showing-value]]
     new-showing-value))                  ;; return new state for the path

;; usage:  (dispatch [:add-todo  "a description string"])
(reg-event-db                     ;; given the text, create a new todo
 :add-todo

  ;; Use the standard interceptors, defined above, which we
  ;; use for all todos-modifying event handlers. Looks after
  ;; writing todos to LocalStore, etc.
 todo-interceptors

  ;; The event handler function.
  ;; The "path" interceptor in `todo-interceptors` means 1st parameter is the
  ;; value at `:todos` path within `db`, rather than the full `db`.
  ;; And, further, it means the event handler returns just the value to be
  ;; put into the `[:todos]` path, and not the entire `db`.
  ;; So, again, a path interceptor acts like clojure's `update-in`
 (fn [todos [_ text]]
   (let [id (allocate-next-id todos)]
     (assoc todos id {:id id :title text :done false}))))

(reg-event-db
 :toggle-done
 todo-interceptors
 (fn [todos [_ id]]
   (update-in todos [id :done] not)))

(reg-event-db
 :save
 todo-interceptors
 (fn [todos [_ id title]]
   (assoc-in todos [id :title] title)))

(reg-event-db
 :delete-todo
 todo-interceptors
 (fn [todos [_ id]]
   (dissoc todos id)))

(reg-event-db
 :clear-completed
 todo-interceptors
 (fn [todos _]
   (let [done-ids (->> (vals todos)         ;; which todos have a :done of true
                       (filter :done)
                       (map :id))]
     (reduce dissoc todos done-ids))))      ;; delete todos which are done

(reg-event-db
 :complete-all-toggle
 todo-interceptors
 (fn [todos _]
   (let [new-done (not-every? :done (vals todos))]   ;; work out: toggle true or false?
     (reduce #(assoc-in %1 [%2 :done] new-done)
             todos
             (keys todos)))))

;; TODO: I'd like to do this.
;; I think it requires the :alpha subscription to have a :set method.
#_(reg :event :toggle-alpha :<- :alpha? :-> not)

(reg-event-db
 :toggle-alpha
 (fn [db _]
   ;; don't need to know the path any more.
   ;; can compute the sub instead.
   ;; it's memory-safe.
   (let [alpha? @(sub :alpha?)]
     ;; still need to know the path to update the value.
     (update-in db [:very :long :path :alpha?] not))))
````

## File: examples/todomvc/src/todomvc/subs.cljs
````clojure
(ns todomvc.subs
  (:require [re-frame.alpha :refer [reg-sub subscribe reg]]))

;; -------------------------------------------------------------------------------------
;; Layer 2
;;
;; See https://day8.github.io/re-frame/subscriptions/
;;
;; Layer 2 query functions are "extractors". They take from `app-db`
;; and don't do any further computation on the extracted values. Any further
;; computation should happen in Layer 3.
;; Why?  It is an efficiency thing. Every Layer 2 subscription will rerun any time
;; that `app-db` changes (in any way). As a result, we want Layer 2 to be trivial.
;;
(reg-sub
 :showing          ;; usage:   (subscribe [:showing])
 (fn [db _]        ;; db is the (map) value stored in the app-db atom
   (:showing db))) ;; extract a value from the application state

;; Next, the registration of a similar handler is done in two steps.
;; First, we `defn` a pure handler function.  Then, we use `reg-sub` to register it.
;; Two steps. This is different to that first registration, above, which was done
;; in one step using an anonymous function.
(defn sorted-todos
  [db _]
  (:todos db))
(reg-sub :sorted-todos sorted-todos)    ;; usage: (subscribe [:sorted-todos])

;; -------------------------------------------------------------------------------------
;; Layer 3
;;
;; See https://day8.github.io/re-frame/subscriptions/
;;
;; A subscription handler is a function which is re-run when its input signals
;; change. Each time it is rerun, it produces a new output (return value).
;;
;; In the simple case, app-db is the only input signal, as was the case in the two
;; simple subscriptions above. But many subscriptions are not directly dependent on
;; app-db, and instead, depend on a value derived from app-db.
;;
;; Such handlers represent "intermediate nodes" in a signal graph.  New values emanate
;; from app-db, and flow out through a signal graph, into and out of these intermediate
;; nodes, before a leaf subscription delivers data into views which render data as hiccup.
;;
;; When writing and registering the handler for an intermediate node, you must nominate
;; one or more input signals (typically one or two).
;;
;; reg-sub allows you to supply:
;;
;;   1. a function which returns the input signals. It can return either a single signal or
;;      a vector of signals, or a map where the values are the signals.
;;
;;   2. a function which does the computation. It takes input values and produces a new
;;      derived value.
;;
;; In the two simple examples at the top, we only supplied the 2nd of these functions.
;; But now we are dealing with intermediate (layer 3) nodes, we'll need to provide both fns.
;;
(reg-sub
 :todos        ;; usage:   (subscribe [:todos])

  ;; This function returns the input signals.
  ;; In this case, it returns a single signal.
  ;; Although not required in this example, it is called with two parameters
  ;; being the two values supplied in the originating `(subscribe X Y)`.
  ;; X will be the query vector and Y is an advanced feature and out of scope
  ;; for this explanation.
 (fn [query-v _]
   (subscribe [:sorted-todos]))    ;; returns a single input signal

  ;; This 2nd fn does the computation. Data values in, derived data out.
  ;; It is the same as the two simple subscription handlers up at the top.
  ;; Except they took the value in app-db as their first argument and, instead,
  ;; this function takes the value delivered by another input signal, supplied by the
  ;; function above: (subscribe [:sorted-todos])
  ;;
  ;; Subscription handlers can take 3 parameters:
  ;;  - the input signals (a single item, a vector or a map)
  ;;  - the query vector supplied to query-v  (the query vector argument
  ;; to the "subscribe") and the 3rd one is for advanced cases, out of scope for this discussion.
 (fn [sorted-todos query-v _]
   (vals sorted-todos)))

;; So here we define the handler for another intermediate node.
;; This time the computation involves two input signals.
;; As a result note:
;;   - the first function (which returns the signals) returns a 2-vector
;;   - the second function (which is the computation) destructures this 2-vector as its first parameter
(reg-sub
 :visible-todos

  ;; Signal Function
  ;; Tells us what inputs flow into this node.
  ;; Returns a vector of two input signals (in this case)
 (fn [query-v _]
   [(subscribe [:todos])
    (subscribe [:showing])])

  ;; Computation Function
 (fn [[todos showing] _]   ;; that 1st parameter is a 2-vector of values
   (let [filter-fn (case showing
                     :active (complement :done)
                     :done   :done
                     :all    identity)]
     (filter filter-fn todos))))

;; -------------------------------------------------------------------------------------
;; Hey, wait on!!
;;
;; How did those two simple Layer 2 registrations at the top work?
;; We only supplied one function in those registrations, not two?
;; Very observant of you, I'm glad you asked.
;; When the signal-returning-fn is omitted, reg-sub provides a default,
;; and it looks like this:
;;    (fn [_ _]
;;       re-frame.db/app-db)
;; It returns one signal, and that signal is app-db itself.
;;
;; So the two simple registrations at the top didn't need to provide a signal-fn,
;; because they operated only on the value in app-db, supplied as 'db' in the 1st argument.
;;
;; So that, by the way, is why Layer 2 subscriptions always re-calculate when `app-db`
;; changes - `app-db` is literally their input signal.

;; -------------------------------------------------------------------------------------
;; SUGAR ?
;; Now for some syntactic sugar...
;; The purpose of the sugar is to remove boilerplate noise. To distill to the essential
;; in 90% of cases.
;; Because it is so common to nominate 1 or more input signals,
;; reg-sub provides some macro sugar so you can nominate a very minimal
;; vector of input signals. The 1st function is not needed.
;; Here is the example above rewritten using the sugar.
#_(reg-sub
   :visible-todos
   :<- [:todos]
   :<- [:showing]
   (fn [[todos showing] _]
     (let [filter-fn (case showing
                       :active (complement :done)
                       :done   :done
                       :all    identity)]
       (filter filter-fn todos))))

(reg-sub
 :all-complete?
 :<- [:todos]
 (fn [todos _]
   (every? :done todos)))

(reg-sub
 :completed-count
 :<- [:todos]
 (fn [todos _]
   (count (filter :done todos))))

(reg-sub
 :footer-counts
 :<- [:todos]
 :<- [:completed-count]
 (fn [[todos completed] _]
   [(- (count todos) completed) completed]))

(reg :sub :alpha? :-> (comp :alpha? :path :long :very))
````

## File: examples/todomvc/src/todomvc/views.cljs
````clojure
(ns todomvc.views
  (:require [reagent.core  :as reagent]
            [re-frame.alpha :refer [subscribe dispatch sub]]
            [clojure.string :as str]))

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [val  (reagent/atom title)
        stop #(do (reset! val "")
                  (when on-stop (on-stop)))
        save #(let [v (-> @val str str/trim)]
                (on-save v)
                (stop))]
    (fn [props]
      [:input (merge (dissoc props :on-save :on-stop :title)
                     {:type        "text"
                      :value       @val
                      :auto-focus  true
                      :on-blur     save
                      :on-change   #(reset! val (-> % .-target .-value))
                      :on-key-down #(case (.-which %)
                                      13 (save)
                                      27 (stop)
                                      nil)})])))

(defn todo-item
  []
  (let [editing (reagent/atom false)]
    (fn [{:keys [id done title]}]
      [:li {:class (str (when done "completed ")
                        (when @editing "editing"))}
       [:div.view
        [:input.toggle
         {:type "checkbox"
          :checked done
          :on-change #(dispatch [:toggle-done id])}]
        [:label
         {:on-double-click #(reset! editing true)}
         title]
        [:button.destroy
         {:on-click #(dispatch [:delete-todo id])}]]
       (when @editing
         [todo-input
          {:class "edit"
           :title title
           :on-save #(if (seq %)
                       (dispatch [:save id %])
                       (dispatch [:delete-todo id]))
           :on-stop #(reset! editing false)}])])))

(defn task-list
  []
  (let [visible-todos @(subscribe [:visible-todos])
        all-complete? @(subscribe [:all-complete?])]
    [:section#main
     [:input#toggle-all
      {:type "checkbox"
       :checked all-complete?
       :on-change #(dispatch [:complete-all-toggle])}]
     [:label
      {:for "toggle-all"}
      "Mark all as complete"]
     [:ul#todo-list
      (for [todo  visible-todos]
        ^{:key (:id todo)} [todo-item todo])]]))

(defn footer-controls
  []
  (let [[active done] @(subscribe [:footer-counts])
        showing       @(subscribe [:showing])
        a-fn          (fn [filter-kw txt]
                        [:a {:class (when (= filter-kw showing) "selected")
                             :href (str "#/" (name filter-kw))} txt])]
    [:footer#footer
     [:span#todo-count
      [:strong active] " " (case active 1 "item" "items") " left"]
     [:ul#filters
      [:li (a-fn :all    "All")]
      [:li (a-fn :active "Active")]
      [:li (a-fn :done   "Completed")]]
     (when (pos? done)
       [:button#clear-completed {:on-click #(dispatch [:clear-completed])}
        "Clear completed"])]))

(defn task-entry
  []
  [:header#header
   [:h1 "todos"]
   [todo-input
    {:id "new-todo"
     :placeholder "What needs to be done?"
     :on-save #(when (seq %)
                 (dispatch [:add-todo %]))}]])

(defn alpha []
  (let [alpha? @(sub :alpha?)]
    [:a {:href "#"
         :style {:color (if alpha? "red" "gray")}
         :on-click #(dispatch [:toggle-alpha])}
     (if alpha? "alpha is running!" "try alpha?")]))

(defn todo-app
  []
  [:<>
   [alpha]
   [:section#todoapp
    [task-entry]
    (when (seq @(subscribe [:todos]))
      [task-list])
    [footer-controls]]
   [:footer#info
    [:p "Double-click to edit a todo"]]])
````

## File: src/re_frame/flow/alpha.cljc
````
(ns re-frame.flow.alpha
  (:require
   #?(:cljs [re-frame.db :as db])
   [re-frame.utils :as u]
   [re-frame.registrar :refer [get-handler]]
   [re-frame.loggers     :refer [console]]
   [re-frame.interceptor :refer [->interceptor get-effect get-coeffect
                                 assoc-effect update-effect]]
   [re-frame.interop :as interop]
   #?(:cljs [reagent.core :as r])))

(def db-path? vector?)

(def flow? map?)

(def flow<-? (comp some? ::flow<-))

(def flows (interop/ratom {}))

(defn lookup [id] (get @flows id))

(defn input-ids [{:keys [inputs live-inputs]}]
  (vec (distinct (into []
                       (comp (remove db-path?)
                             (map #(or (::flow<- %) %)))
                       (concat (vals inputs) (vals live-inputs))))))

(defn topsort [flows]
  (->> flows
       (u/map-vals input-ids)
       u/remove-orphans
       u/topsort-kahn
       reverse
       (map flows)))

(def topsort* (memoize topsort))

(defn default [id]
  {:id id
   :path [id]
   :inputs {}
   :output (constantly true)
   :live? (constantly true)
   :live-inputs {}
   :cleanup u/deep-dissoc})

(defn stale-in-flows [flows {:keys [inputs]}]
  (reduce-kv (fn [m k {:keys [path]}]
               (cond-> m
                 (contains? (set (vals inputs)) path) (assoc k path)))
             {}
             flows))

(defn stale-out-flows [flows {:keys [path]}]
  (reduce-kv (fn [m k {:keys [inputs]}]
               (let [bad-inputs (into {} (filter (comp #{path} val)) inputs)]
                 (cond-> m (seq bad-inputs) (assoc k bad-inputs))))
             {}
             flows))

(defn validate-inputs [{:keys [inputs]}]
  (doseq [[_ input] inputs
          :when (not ((some-fn db-path? flow<-?) input))]
    (throw (#?(:clj Exception. :cljs js/Error.) "bad input"))))

(defn warn-stale-dependencies [flows new-flow]
  (let [ins (stale-in-flows flows new-flow)
        outs (stale-out-flows flows new-flow)
        warn-ins (fn [[id path]]
                   ["- Input" (str path)
                    "matches the output path of" (str id) ".\n"
                    "  For an explicit dependency, change it to (re-frame/flow<-"
                    (str id ").") "\n"])
        warn-outs (fn [[id inputs]]
                    (mapcat (fn [[input-id _]]
                              ["- Output" (str (:path new-flow))
                               "matches the input" (str input-id)
                               "of the flow" (str id ".\n")
                               "  For an explicit dependency, change that input to"
                               "(re-frame/flow<-" (str (:id new-flow) ").") "\n"])
                            inputs))
        warnings (concat (mapcat warn-ins ins) (mapcat warn-outs outs))]
    (when (seq warnings)
      (apply console :warn "Warning: You called `reg-flow` with the flow" (str (:id new-flow))
             "but this created stale dependencies.\n"
             "Your flows may not evaluate in the correct order.\n"
             warnings))))

(defn reg-flow
  ([k m]
   (reg-flow (assoc m :id k)))
  ([m]
   (validate-inputs m)
   (warn-stale-dependencies @flows m)
   (swap! flows assoc
          (:id m) (with-meta (merge (default (:id m)) m)
                    (merge
                     {::new? true}
                     #?(:cljs
                        {::ref (r/reaction (get-in @db/app-db (:path m)))}))))))

(defn clear-flow
  ([]
   (swap! flows vary-meta update ::cleared into @flows)
   (swap! flows empty))
  ([id]
   (when-let [flow (lookup id)]
     (swap! flows dissoc id)
     (swap! flows vary-meta update ::cleared assoc (:id flow) flow))))

(defn flow<- [id] {::flow<- id})

(def flow-fx-ids #{:reg-flow :clear-flow})

(defn do-effect [[k v]] ((get-handler :fx k false) v))

(def remove-fx (partial remove (comp flow-fx-ids first)))

(def dissoc-fx #(apply dissoc % flow-fx-ids))

(def do-fx
  (->interceptor
   {:id :do-flow-fx
    :after (fn [{{:keys [fx] :as effects} :effects
                 :as ctx}]
             (let [flow-fx (concat (select-keys effects flow-fx-ids)
                                   (filterv (comp flow-fx-ids first) fx))]
               (doall (map do-effect flow-fx))
               (-> ctx
                   (update-in [:effects :fx] remove-fx)
                   (update :effects dissoc-fx))))}))

(defn resolve-input [db input]
  (if (vector? input)
    (get-in db input)
    (some->> input ::flow<- lookup :path (resolve-input db))))

(defn resolve-inputs [db inputs]
  (if (empty? inputs) db (u/map-vals (partial resolve-input db) inputs)))

(defn run [ctx {:as     flow
                :keys   [path cleanup live? inputs live-inputs output id]
                flow-fx :fx
                ::keys  [cleared?]}]
  (let [{::keys [new?]} (meta flow)
        old-db          (get-coeffect ctx :db)
        db              (or (get-effect ctx :db) old-db)
        fx              (get-effect ctx :fx)

        id->old-in (resolve-inputs old-db inputs)
        id->in     (resolve-inputs db inputs)
        dirty?     (not= id->in id->old-in)

        id->old-live-in (resolve-inputs old-db live-inputs)
        id->live-in     (resolve-inputs db live-inputs)

        old-output      (get-in old-db path)

        bardo           [(cond new? :new (live? id->old-live-in) :live :else :dead)
                         (cond cleared? :cleared (live? id->live-in) :live :else :dead)]

        new-db (case bardo
                 [:live :live]    (cond-> db dirty? (assoc-in path (output id->in id->old-in old-output)))
                 [:live :dead]    (cleanup db path)
                 [:dead :live]    (assoc-in db path (output id->in id->old-in old-output))
                 [:new :live]     (do (swap! flows update id vary-meta dissoc ::new?)
                                      (assoc-in db path (output id->in id->old-in old-output)))
                 [:live :cleared] (cleanup db path)
                 nil)

        new-fx (when flow-fx
                 (case bardo
                   [:live :live] (when dirty? (concat fx (flow-fx id->in id->old-in old-output)))
                   [:dead :live] (concat fx (flow-fx id->in id->old-in old-output))
                   [:new :live]  (concat fx (flow-fx id->in id->old-in old-output))
                   nil))]
    (cond-> ctx
      new-db (assoc-effect :db new-db)
      new-fx (assoc-effect :fx new-fx))))

(defn with-cleared [m]
  (into m (map (fn [[k v]] [[::cleared k (gensym)] (assoc v ::cleared? true)])
               (::cleared (meta m)))))

(def interceptor
  (->interceptor
   {:id    :flow
    :after (comp (fn [ctx]
                   (let [all-flows (with-cleared @flows)]
                     (swap! flows vary-meta dissoc ::cleared)
                     (reduce run ctx (topsort all-flows))))
                 (fn [{{:keys [db]} :effects :as ctx}]
                   (assoc ctx :re-frame/pre-flow-db db)))}))
````

## File: src/re_frame/query/alpha.cljc
````
(ns re-frame.query.alpha
  (:require
   [re-frame :as-alias rf]
   [re-frame.db :refer [app-db]]
   [re-frame.interop :refer [reagent-id]]
   [re-frame.loggers :refer [console]]
   [re-frame.register.alpha :refer [lifecycle->method]]
   [re-frame.registrar :refer [get-handler]]
   [re-frame.trace :as trace :include-macros true]))

(declare lifecycle)

(defn legacy-lifecycle [v]
  (when (vector? v)
    (or (lifecycle (meta v))
        :default)))

(defn legacy-query-id [q]
  (when (vector? q) (first q)))

(def id (some-fn legacy-query-id ::rf/q))

(def flow-lifecycle (comp #{:flow} id))

(def lifecycle (some-fn flow-lifecycle
                        legacy-lifecycle
                        ::rf/lifecycle
                        (constantly :default)))

(defn method [q] (@lifecycle->method (lifecycle q)))

(defn clear-all-methods! [] (reset! lifecycle->method {}))

(def cache (atom {}))

(defn cached [q] (if-some [r (get-in @cache [(lifecycle q) q])]
                   (do (trace/merge-trace! {:tags {:cached? true
                                                   :reaction (reagent-id r)}})
                       r)
                   (trace/merge-trace! {:tags {:cached? false}})))

(defn cache! [q r] (swap! cache assoc-in [(lifecycle q) q] r) r)

(defn clear!
  ([] (reset! cache {}))
  ([q] (clear! q (lifecycle q)))
  ([q strat] (swap! cache update strat dissoc q)))

(defn handle [q]
  (let [handler (get-handler :sub (id q))]
    (if-not (nil? handler)
      (handler app-db q)
      (do (trace/merge-trace! {:error true})
          (console :error
                   "re-frame: no subscription handler registered for: "
                   (id q)
                   ". Returning a nil subscription.")))))

(defn query? [q]
  (some? (and (id q)
              (lifecycle q))))
````

## File: src/re_frame/subs/alpha.cljc
````
(ns re-frame.subs.alpha
  (:require
   [re-frame.subs :refer [deref-input-signals sugar warn-when-not-reactive]]
   [re-frame.registrar :refer [register-handler]]
   [re-frame.register.alpha :refer [reg lifecycle->method]]
   [re-frame.interop :refer [add-on-dispose! make-reaction reactive? reagent-id ratom]]
   [re-frame.query.alpha :as q]
   [re-frame :as-alias rf]
   [re-frame.trace :as trace :include-macros true]
   [re-frame.flow.alpha :as flow]))

(defmethod reg :sub-lifecycle [_ k f]
  (swap! lifecycle->method assoc
         k
         (fn [q]
           (trace/with-trace {:operation (q/id q)
                              :op-type :sub/create
                              :tags {:query q}}
             (f q)))))

(defn sub
  ([q]
   (if (keyword? q)
     (sub q {})
     (let [md (q/method q)]
       (cond (map? q) (md q)
             (vector? q) (md {::rf/q (q/id q)
                              ::rf/lifecycle (q/lifecycle q)
                              ::rf/query-v q})))))
  ([id q]
   (sub (assoc q ::rf/q id))))

(defmethod reg :sub [kind id & args]
  (let [[inputs-fn computation-fn] (apply sugar id sub q/query? args)]
    (register-handler
     kind
     id
     (fn subs-handler-fn [_ q]
       (let [subscriptions (inputs-fn q nil)
             rid (atom nil)
             r (make-reaction
                #(trace/with-trace {:operation (q/id q)
                                    :op-type   :sub/run
                                    :tags      {:query      q
                                                :reaction   @rid}}
                   (let [subscription (computation-fn
                                       (deref-input-signals subscriptions id)
                                       q)]
                     (trace/merge-trace! {:tags {:value subscription}})
                     subscription)))]
         (reset! rid (reagent-id r))
         r)))))

(defmethod reg :legacy-sub [_ id & args]
  (let [[inputs-fn computation-fn] (apply sugar id sub q/query? args)]
    (register-handler
     :sub
     id
     (fn subs-handler-fn [_ q]
       (let [subscriptions (inputs-fn q nil)
             rid (atom nil)
             r (make-reaction
                #(trace/with-trace {:operation (q/id q)
                                    :op-type   :sub/run
                                    :tags      {:query      q
                                                :reaction   @rid}}
                   (let [q (if (map? q)
                             (-> (or (::rf/query-v q) [(q/id q)])
                                 (vary-meta assoc ::rf/lifecycle (q/lifecycle q)))
                             q)
                         subscription (computation-fn
                                       (deref-input-signals subscriptions id)
                                       q)]
                     (trace/merge-trace! {:tags {:value subscription}})
                     subscription)))]
         (reset! rid (reagent-id r))
         r)))))

(defn sub-reactive [q]
  (warn-when-not-reactive)
  (or (q/cached q)
      (let [md (q/lifecycle q)
            r (q/handle q)]
        (add-on-dispose! r #(q/clear! q md))
        (q/cache! q r))))

(reg :sub-lifecycle :reactive sub-reactive)

(defn sub-safe [q]
  (if (reactive?)
    (sub-reactive q)
    (or (q/cached q)
        (q/handle q))))

(reg :sub-lifecycle :safe sub-safe)
(reg :sub-lifecycle :default sub-safe)

(defn sub-forever [q]
  (or (q/cached q)
      (q/cache! q (q/handle q))))

(reg :sub-lifecycle :forever sub-forever)

(def nil-ref (ratom nil))

(defn sub-flow [q]
  (or (some-> (:id (or (second (::rf/query-v q)) q))
              flow/lookup meta :re-frame.flow.alpha/ref)
      nil-ref))

(reg :sub-lifecycle :flow sub-flow)
````

## File: src/re_frame/cofx.cljc
````
(ns re-frame.cofx
  (:require
   [re-frame.db           :refer [app-db]]
   [re-frame.interceptor  :refer [->interceptor]]
   [re-frame.registrar    :refer [get-handler register-handler]]
   [re-frame.loggers      :refer [console]]))

;; -- Registration ------------------------------------------------------------

(def kind :cofx)
(assert (re-frame.registrar/kinds kind))

(defn reg-cofx
  [id handler]
  (register-handler kind id handler))

;; -- Interceptor -------------------------------------------------------------

(defn inject-cofx
  ([id]
   (->interceptor
    :id      :coeffects
    :before  (fn coeffects-before
               [context]
               (if-let [handler (get-handler kind id)]
                 (update context :coeffects handler)
                 (console :error "No cofx handler registered for" id)))))
  ([id value]
   (->interceptor
    :id     :coeffects
    :before  (fn coeffects-before
               [context]
               (if-let [handler (get-handler kind id)]
                 (update context :coeffects handler value)
                 (console :error "No cofx handler registered for" id))))))

;; -- Builtin CoEffects Handlers  ---------------------------------------------

;; :db
;;
;; Adds to coeffects the value in `app-db`, under the key `:db`
(reg-cofx
 :db
 (fn db-coeffects-handler
   [coeffects]
   (assoc coeffects :db @app-db)))

;; Because this interceptor is used so much, we reify it
(def inject-db (inject-cofx :db))
````

## File: src/re_frame/core.cljc
````
(ns re-frame.core
  (:require
   [re-frame.events           :as events]
   [re-frame.subs             :as subs]
   [re-frame.interop          :as interop]
   [re-frame.db               :as db]
   [re-frame.fx               :as fx]
   [re-frame.cofx             :as cofx]
   [re-frame.router           :as router]
   [re-frame.settings         :as settings]
   [re-frame.loggers          :as loggers]
   [re-frame.registrar        :as registrar]
   [re-frame.interceptor      :as interceptor]
   [re-frame.std-interceptors :as std-interceptors :refer [db-handler->interceptor
                                                           fx-handler->interceptor
                                                           ctx-handler->interceptor]]
   [re-frame.utils            :as utils]
   [clojure.set               :as set]))

;; -- dispatch ----------------------------------------------------------------

(defn dispatch
  "Queue `event` for processing (handling).

  `event` is a vector and the first element is typically a keyword
  which identifies the kind of event.

  The event will be added to a FIFO processing queue, so event
  handling does not happen immediately. It will happen 'very soon'
  but not now. And if the queue already contains events, they
  will be processed first.

  Usage:

      #!clj
      (dispatch [:order \"pizza\" {:supreme 2 :meatlovers 1 :veg 1}])
  "
  {:api-docs/heading "Dispatching Events"}
  [event]
  (router/dispatch event))

(defn dispatch-sync
  "Synchronously (immediately) process `event`. It does **not** queue
  the event for handling later as `dispatch` does.

  `event` is a vector and the first element is typically a keyword
  which identifies the kind of event.

  It is an error to use `dispatch-sync` within an event handler because
  you can't immediately process an new event when one is already
  part way through being processed.

  Generally, avoid using this function, and instead, use `dispatch`.
  Only use it in the narrow set of cases where any delay in
  processing is a problem:

    1. the `:on-change` handler of a text field where we are expecting fast typing
    2. when initialising your app - see 'main' in examples/todomvc/src/core.cljs
    3. in a unit test where immediate, synchronous processing is useful

  Usage:

      #!clj
      (dispatch-sync [:sing :falsetto \"piano accordion\"])
  "
  {:api-docs/heading "Dispatching Events"}
  [event]
  (router/dispatch-sync event))

;; -- Events ------------------------------------------------------------------

(defn reg-event-db
  "Register the given event `handler` (function) for the given `id`. Optionally, provide
  an `interceptors` chain:

    - `id` is typically a namespaced keyword  (but can be anything)
    - `handler` is a function: (db event) -> db
    - `interceptors` is a collection of interceptors. Will be flattened and nils removed.

  Example Usage:

      #!clj
      (reg-event-db
        :token
        (fn [db event]
          (assoc db :some-key (get event 2)))  ;; return updated db

  Or perhaps:

      #!clj
      (reg-event-db
        :namespaced/id           ;; <-- namespaced keywords are often used
        [one two three]          ;; <-- a seq of interceptors
        (fn [db [_ arg1 arg2]]   ;; <-- event vector is destructured
          (-> db
            (dissoc arg1)
            (update :key + arg2))))   ;; return updated db
  "
  {:api-docs/heading "Event Handlers"}
  ([id handler]
   (reg-event-db id nil handler))
  ([id interceptors handler]
   (events/register id [cofx/inject-db fx/do-fx std-interceptors/inject-global-interceptors interceptors (db-handler->interceptor handler)])))

(defn reg-event-fx
  "Register the given event `handler` (function) for the given `id`. Optionally, provide
  an `interceptors` chain:

    - `id` is typically a namespaced keyword  (but can be anything)
    - `handler` is a function: (coeffects-map event-vector) -> effects-map
    - `interceptors` is a collection of interceptors. Will be flattened and nils removed.


  Example Usage:

      #!clj
      (reg-event-fx
        :event-id
        (fn [cofx event]
          {:db (assoc (:db cofx) :some-key (get event 2))}))   ;; return a map of effects


  Or perhaps:

      #!clj
      (reg-event-fx
        :namespaced/id           ;; <-- namespaced keywords are often used
        [one two three]          ;; <-- a seq of interceptors
        (fn [{:keys [db] :as cofx} [_ arg1 arg2]] ;; destructure both arguments
          {:db (assoc db :some-key arg1)          ;; return a map of effects
           :fx [[:dispatch [:some-event arg2]]]}))
  "
  {:api-docs/heading "Event Handlers"}
  ([id handler]
   (reg-event-fx id nil handler))
  ([id interceptors handler]
   (events/register id [cofx/inject-db fx/do-fx std-interceptors/inject-global-interceptors interceptors (fx-handler->interceptor handler)])))

(defn reg-event-ctx
  "Register the given event `handler` (function) for the given `id`. Optionally, provide
  an `interceptors` chain:

    - `id` is typically a namespaced keyword  (but can be anything)
    - `handler` is a function: context-map -> context-map

  You can explore what is provided in `context` [here](https://day8.github.io/re-frame/Interceptors/#what-is-context).

  Example Usage:

      #!clj
      (reg-event-ctx
        :event-id
        (fn [{:keys [coeffects] :as context}]
          (let [initial  {:db     (:db coeffects)
                          :event  (:event coeffects)
                          :fx     []}
                result   (-> initial
                             function1
                             function2
                             function3)
                effects  (select-keys result [:db :fx])]
             (assoc context :effects effects))))
  "
  {:api-docs/heading "Event Handlers"}
  ([id handler]
   (reg-event-ctx id nil handler))
  ([id interceptors handler]
   (events/register id [cofx/inject-db fx/do-fx std-interceptors/inject-global-interceptors interceptors (ctx-handler->interceptor handler)])))

(defn clear-event
  "Unregisters event handlers (presumably registered previously via the use of `reg-event-db` or `reg-event-fx`).

  When called with no args, it will unregister all currently registered event handlers.

  When given one arg, assumed to be the `id` of a previously registered
  event handler, it will unregister the associated handler. Will produce a warning to
  console if it finds no matching registration."
  {:api-docs/heading "Event Handlers"}
  ([]
   (registrar/clear-handlers events/kind))
  ([id]
   (registrar/clear-handlers events/kind id)))

;; -- subscriptions -----------------------------------------------------------

(defn reg-sub
  "A call to `reg-sub` associates a `query-id` WITH two functions.

  The two functions provide 'a mechanism' for creating a node
  in the Signal Graph. When a node of type `query-id` is needed,
  the two functions can be used to create it.

  The three arguments are:

    - `query-id` - typically a namespaced keyword (later used in subscribe)
    - optionally, an `input signals` function which returns the input data
      flows required by this kind of node.
    - a `computation function` which computes the value (output) of the
      node (from the input data flows)

  Later, during app execution, a call to `(subscribe [:sub-id 3 :blue])`,
  will trigger the need for a new `:sub-id` Signal Graph node (matching the
  query `[:sub-id 3 :blue]`). And, to create that node the two functions
  associated with `:sub-id` will be looked up and used.

  Just to be clear: calling `reg-sub` does not immediately create a node.
  It only registers 'a mechanism' (the two functions) by which nodes
  can be created later, when a node is bought into existence by the
  use of `subscribe` in a `View Function`.

  `reg-sub` arguments are:

    - a `query-id` (typically a namespaced keyword)
    - a function which returns the inputs required by this kind of node (can be supplied  in one of three ways)
    - a function which computes the value of this kind of node (can be supplied in one of three ways)

  The `computation function` is always the last argument supplied and has three ways to be called.
  Two of these methods are syntactic sugar to provide easier access to functional abstractions around your data.

  1. A function that will accept two parameters, the `input-values` and `query-vector`. This is the
     standard way to provide a `computation-function`

          #!clj
          (reg-sub
            :query-id
            (fn [input-values query-vector]
              (:foo input-values)))

  2. A single sugary tuple of `:->` and a 1-arity `computation-function`:

          #!clj
          (reg-sub
            :query-id
            :-> computation-fn)

      This sugary variation allows you to pass a function that will expect only one parameter,
      namely the `input-values` and entirely omit the `query-vector`. A typical `computation-function`
      expects two parameters which can cause unfortunate results when attempting to use
      clojure standard library functions, or other functions, in a functional manner.

      For example, a significant number of subscriptions exist only to get a value
      from the `input-values`. As shown below, this subscription will simply retrieve
      the value associated with the `:foo` key in our db:

          #!clj
          (reg-sub
            :query-id
            (fn [db _]    ;; :<---- trivial boilerplate we might want to skip over
              (:foo db)))

      This is slightly more boilerplate than we might like to do,
      as we can use a keyword directly as a function, and we might like to do this:

          #!clj
          (reg-sub
            :query-id
            :foo)  ;; :<---- This could be dangerous. If `:foo` is not in db, we get the `query-vector` instead of `nil`.

      By using `:->` our function would not contain the `query-vector`, and any
      missing keys would be represented as such:

          #!clj
          (reg-sub
            :query-id
            :-> :foo)

      This form allows us to ignore the `query-vector` if our `computation-function`
      has no need for it, and be safe from any accidents. Any 1-arity function can be provided,
      and for more complicated use cases, `partial`, `comp`, and anonymous functions can still be used.

  3. A single sugary tuple of `:=>` and a multi-arity `computation-function`

          #!clj
          (reg-sub
            :query-id
            :=> computation-fn)

      The `query-vector` can be broken into two components `[query-id & optional-values]`, and
      some subscriptions require the `optional-values` for extra work within the subscription.
      To use them in variation #1, we need to destructure our `computation-function` parameters
      in order to use them.

          #!clj
          (reg-sub
            :query-id
            (fn [db [_ foo]]
              [db foo]))

      Again we are writing boilerplate just to reach our values, and we might prefer to
      have direction access through a parameter vector like `[input-values optional-values]`
      instead, so we might be able to use a multi-arity function directly as our `computation-function`.
      A rewrite of the above sub using this sugary syntax would look like this:

          #!clj
          (reg-sub
            :query-id
            :=> vector)  ;; :<---- Could also be `(fn [db foo] [db foo])`

  The `computation function` is expected to take two arguments:

    - `input-values` - the values which flow into this node (how is it wired into the graph?)
    - `query-vector` - the vector given to `subscribe`

  and it returns a computed value (which then becomes the output of the node)

  When `computation function` is called, the 2nd `query-vector` argument will be that
  vector supplied to the `subscribe`. So, if the call was `(subscribe [:sub-id 3 :blue])`,
  then the `query-vector` supplied to the computation function will be `[:sub-id 3 :blue]`.

  The argument(s) supplied to `reg-sub` between `query-id` and the `computation-function`
  can vary in 3 ways, but whatever is there defines the `input signals` part
  of `the mechanism`, specifying what input values \"flow into\" the
  `computation function` (as the 1st argument) when it is called.

  So, `reg-sub` can be called in one of three ways, because there are three ways
  to define the input signals part. But note, the 2nd method, in which a
  `signals function` is explicitly supplied, is the most canonical and
  instructive. The other two are really just sugary variations.

  **First variation** - no input signal function given:

      #!clj
      (reg-sub
        :query-id
        a-computation-fn)   ;; has signature:  (fn [db query-vec]  ... ret-value)

     In the absence of an explicit `signals function`, the node's input signal defaults to `app-db`
     and, as a result, the value within `app-db` (a map) is
     given as the 1st argument when `a-computation-fn` is called.


  **Second variation** - a signal function is explicitly supplied:

      #!clj
      (reg-sub
        :query-id
        signal-fn     ;; <-- here
        computation-fn)

  This is the most canonical and instructive of the three variations.

  When a node is created from the template, the `signal function` will be called and it
  is expected to return the input signal(s) as either a singleton, if there is only
  one, or a sequence if there are many, or a map with the signals as the values.

  The current values of the returned signals will be supplied as the 1st argument to
  the `a-computation-fn` when it is called - and subject to what this `signal-fn` returns,
  this value will be either a singleton, sequence or map of them (paralleling
  the structure returned by the `signal function`).

  This example `signal function` returns a 2-vector of input signals.

      #!clj
      (fn [query-vec dynamic-vec]
         [(subscribe [:a-sub])
          (subscribe [:b-sub])])

  The associated computation function must be written
  to expect a 2-vector of values for its first argument:

      #!clj
      (fn [[a b] query-vec]     ;; 1st argument is a seq of two values
        ....)

  If, on the other hand, the signal function was simpler and returned a singleton, like this:

      #!clj
      (fn [query-vec dynamic-vec]
        (subscribe [:a-sub]))      ;; <-- returning a singleton

  then the associated computation function must be written to expect a single value
  as the 1st argument:

      #!clj
      (fn [a query-vec]       ;; 1st argument is a single value
         ...)

  Further Note: variation #1 above, in which an `signal-fn` was not supplied, like this:

      #!clj
      (reg-sub
        :query-id
        a-computation-fn)   ;; has signature:  (fn [db query-vec]  ... ret-value)

  is the equivalent of using this
  2nd variation and explicitly supplying a `signal-fn` which returns `app-db`:

      #!clj
      (reg-sub
        :query-id
        (fn [_ _]  re-frame/app-db)   ;; <--- explicit signal-fn
        a-computation-fn)             ;; has signature:  (fn [db query-vec]  ... ret-value)

  **Third variation** - syntax Sugar

      #!clj
      (reg-sub
        :a-b-sub
        :<- [:a-sub]
        :<- [:b-sub]
        (fn [[a b] query-vec]    ;; 1st argument is a seq of two values
          {:a a :b b}))

  This 3rd variation is just syntactic sugar for the 2nd.  Instead of providing an
  `signals-fn` you provide one or more pairs of `:<-` and a subscription vector.

  If you supply only one pair a singleton will be supplied to the computation function,
  as if you had supplied a `signal-fn` returning only a single value:

      #!clj
      (reg-sub
        :a-sub
        :<- [:a-sub]
        (fn [a query-vec]      ;; only one pair, so 1st argument is a single value
          ...))

  Syntactic sugar for both the `signal-fn` and `computation-fn` can be used together
  and the direction of arrows shows the flow of data and functions. The example from
  directly above is reproduced here:

      #!clj
      (reg-sub
        :a-b-sub
        :<- [:a-sub]
        :<- [:b-sub]
        :-> (partial zipmap [:a :b]))

  For further understanding, read the tutorials, and look at the detailed comments in
  /examples/todomvc/src/subs.cljs.

  See also: `subscribe`
  "
  {:api-docs/heading "Subscriptions"}
  [query-id & args]
  (apply subs/reg-sub query-id args))

(defn subscribe
  "Given a `query` vector, returns a Reagent `reaction` which will, over
  time, reactively deliver a stream of values. So, in FRP-ish terms,
  it returns a `Signal`.

  To obtain the current value from the Signal, it must be dereferenced:

      #!clj
      (let [signal (subscribe [:items])
            value  (deref signal)]     ;; could be written as @signal
        ...)

   which is typically written tersely as simple:

      #!clj
      (let [items  @(subscribe [:items])]
        ...)


  `query` is a vector of at least one element. The first element is the
  `query-id`, typically a namespaced keyword. The rest of the vector's
  elements are optional, additional values which parameterise the query
  performed.

  `dynv` exists for historical reasons and is borderline deprecated these days.
  It is a vector of signals. Re-frame will dereference each of them and pass a
  vector of their values to your subscription handler as a third argument.
  If there's logic determining __what__ query to subscribe __to__, consider
  expressing it in a `signal function`, or use `reg-sub-raw`. Failing that, `dynv`
  allows you to colocate this logic with the `subscribe` call.

  **Example Usage**:

      #!clj
      (subscribe [:items])
      (subscribe [:items \"blue\" :small])
      (subscribe [:items {:colour \"blue\"  :size :small}])

  Note: for any given call to `subscribe` there must have been a previous call
  to `reg-sub`, registering the query handler (functions) associated with
  `query-id`.

  **Hint**

  When used in a view function BE SURE to `deref` the returned value.
  In fact, to avoid any mistakes, some prefer to define:

      #!clj
      (def <sub  (comp deref re-frame.core/subscribe))

  And then, within their views, they call  `(<sub [:items :small])` rather
  than using `subscribe` directly.

  **De-duplication**

  Two, or more, concurrent subscriptions for the same query will
  source reactive updates from the one executing handler.

  See also: `reg-sub`
  "
  {:api-docs/heading "Subscriptions"}
  ([query]
   (subs/subscribe query))
  ([query dynv]
   (subs/subscribe query dynv)))

(defn clear-sub ;; think unreg-sub
  "Unregisters subscription handlers (presumably registered previously via the use of `reg-sub`).

  When called with no args, it will unregister all currently registered subscription handlers.

  When given one arg, assumed to be the `id` of a previously registered
  subscription handler, it will unregister the associated handler. Will produce a warning to
  console if it finds no matching registration.

  NOTE: Depending on the usecase, it may be necessary to call `clear-subscription-cache!` afterwards"
  {:api-docs/heading "Subscriptions"}
  ([]
   (registrar/clear-handlers subs/kind))
  ([query-id]
   (registrar/clear-handlers subs/kind query-id)))

(defn reg-sub-raw
  "This is a low level, advanced function.  You should probably be
  using `reg-sub` instead.

  Some explanation is available in the docs at
  <a href=\"http://day8.github.io/re-frame/flow-mechanics/\" target=\"_blank\">http://day8.github.io/re-frame/flow-mechanics/</a>"
  {:api-docs/heading "Subscriptions"}
  [query-id handler-fn]
  (registrar/register-handler subs/kind query-id handler-fn))

;; XXX
(defn clear-subscription-cache!
  "Removes all subscriptions from the cache.

  This function can be used at development time or test time. Useful when hot reloading
  namespaces containing subscription handlers. Also call it after a React/render exception,
  because React components won't have been cleaned up properly. And this, in turn, means
  the subscriptions within those components won't have been cleaned up correctly. So this
  forces the issue.
  "
  {:api-docs/heading "Subscriptions"}
  []
  (subs/clear-subscription-cache!))

;; -- effects -----------------------------------------------------------------

(defn reg-fx
  "Register the given effect `handler` for the given `id`:

    - `id` is keyword, often namespaced.
    - `handler` is a side-effecting function which takes a single argument and whose return
      value is ignored.

  To use, first, associate `:effect2` with a handler:

      #!clj
      (reg-fx
         :effect2
         (fn [value]
            ... do something side-effect-y))

  Then, later, if an event handler were to return this effects map:

      #!clj
      {:effect2  [1 2]}

  then the `handler` `fn` we registered previously, using `reg-fx`, will be
  called with an argument of `[1 2]`.
  "
  {:api-docs/heading "Effect Handlers"}
  [id handler]
  (fx/reg-fx id handler))

(defn clear-fx ;; think unreg-fx
  "Unregisters effect handlers (presumably registered previously via the use of `reg-fx`).

  When called with no args, it will unregister all currently registered effect handlers.

  When given one arg, assumed to be the `id` of a previously registered
  effect handler, it will unregister the associated handler. Will produce a warning to
  console if it finds no matching registration.
  "
  {:api-docs/heading "Effect Handlers"}
  ([]
   (registrar/clear-handlers fx/kind))
  ([id]
   (registrar/clear-handlers fx/kind id)))

;; -- coeffects ---------------------------------------------------------------

(defn reg-cofx
  "Register the given coeffect `handler` for the given `id`, for later use
  within `inject-cofx`:

    - `id` is keyword, often namespaced.
    - `handler` is a function which takes either one or two arguments, the first of which is
       always `coeffects` and which returns an updated `coeffects`.

  See also: `inject-cofx`
  "
  {:api-docs/heading "Coeffects"}
  [id handler]
  (cofx/reg-cofx id handler))

(defn inject-cofx
  "Given an `id`, and an optional, arbitrary `value`, returns an interceptor
  whose `:before` adds to the `:coeffects` (map) by calling a pre-registered
  'coeffect handler' identified by the `id`.

  The previous association of a `coeffect handler` with an `id` will have
  happened via a call to `re-frame.core/reg-cofx` - generally on program startup.

  Within the created interceptor, this 'looked up' `coeffect handler` will
  be called (within the `:before`) with two arguments:

  - the current value of `:coeffects`
  - optionally, the originally supplied arbitrary `value`

  This `coeffect handler` is expected to modify and return its first, `coeffects` argument.

  **Example of `inject-cofx` and `reg-cofx` working together**


  First - Early in app startup, you register a `coeffect handler` for `:datetime`:

      #!clj
      (re-frame.core/reg-cofx
        :datetime                        ;; usage  (inject-cofx :datetime)
        (fn coeffect-handler
          [coeffect]
          (assoc coeffect :now (js/Date.))))   ;; modify and return first arg

  Second - Later, add an interceptor to an -fx event handler, using `inject-cofx`:

      #!clj
      (re-frame.core/reg-event-fx            ;; when registering an event handler
        :event-id
        [ ... (inject-cofx :datetime) ... ]  ;; <-- create an injecting interceptor
        (fn event-handler
          [coeffect event]
            ;;... in here can access (:now coeffect) to obtain current datetime ...
          )))

  **Background**

  `coeffects` are the input resources required by an event handler
  to perform its job. The two most obvious ones are `db` and `event`.
  But sometimes an event handler might need other resources.

  Perhaps an event handler needs a random number or a GUID or the current
  datetime. Perhaps it needs access to a DataScript database connection.

  If an event handler directly accesses these resources, it stops being
  pure and, consequently, it becomes harder to test, etc. So we don't
  want that.

  Instead, the interceptor created by this function is a way to 'inject'
  'necessary resources' into the `:coeffects` (map) subsequently given
  to the event handler at call time.

  See also `reg-cofx`
  "
  {:api-docs/heading "Coeffects"}
  ([id]
   (cofx/inject-cofx id))
  ([id value]
   (cofx/inject-cofx id value)))

(defn clear-cofx ;; think unreg-cofx
  "Unregisters coeffect handlers (presumably registered previously via the use of `reg-cofx`).

  When called with no args, it will unregister all currently registered coeffect handlers.

  When given one arg, assumed to be the `id` of a previously registered
  coeffect handler, it will unregister the associated handler. Will produce a warning to
  console if it finds no matching registration."
  {:api-docs/heading "Coeffects"}
  ([]
   (registrar/clear-handlers cofx/kind))
  ([id]
   (registrar/clear-handlers cofx/kind id)))

;; -- error handler ----------------------------------------------------------

(defn reg-event-error-handler
  "Register the given event error `handler` (function) that will catch unhandled exceptions
  thrown in the interceptors/handler chain.

  Only one `handler` can be registered. Registering a new `handler` clears the existing `handler`.

  This `handler` function has the signature:

  `(handler [original-error re-frame-error])`

  - `original-error`: A platform-native Error object.
     Represents the original error thrown by user code.
     this is the error you see when no `handler` is registered.

  - `re-frame-error`: A clojure ExceptionInfo object.
     Includes the stacktrace of re-frame's internal functions,
     and extra data about the interceptor process.
     Call `(ex-data re-frame-error)` to get this info.

     The data includes:

     - `:interceptor`: the `:id` of the throwing interceptor.
     - `:direction`: `:before` or `:after`.
     - `:event-v`: the re-frame event which invoked this interceptor."
  [handler]
  (registrar/register-handler :error :event-handler handler))

(reg-event-error-handler interceptor/default-error-handler)

;; -- interceptors ------------------------------------------------------------

(def ^{:api-docs/heading "Interceptors"} debug
  "An interceptor which logs/instruments an event handler's actions to
  `re-frame/console` at the `:log` level.

  Output includes:

    1. the event vector
    2. a `clojure.data/diff` of db, before vs after, which shows
       the changes caused by the event handler. To understand the output,
       you should understand:
       <a href=\"https://clojuredocs.org/clojure.data/diff\" target=\"_blank\">https://clojuredocs.org/clojure.data/diff</a>.

  You'd typically include this interceptor after (to the right of) any
  `path` interceptor.

  Warning:  calling `clojure.data/diff` on large, complex data structures
  can be slow. So, you won't want this interceptor present in production
  code. So, you should condition it out like this:

      #!clj
      (re-frame.core/reg-event-db
        :evt-id
        [(when ^boolean goog.DEBUG re-frame.core/debug)]  ;; <-- conditional
        (fn [db v]
           ...))

  To make this code fragment work, you'll also have to set `goog.DEBUG` to
  `false` in your production builds. For an example, look in `project.clj` of /examples/todomvc.
  "
  std-interceptors/debug)

(defn path
  "Returns an interceptor which acts somewhat like `clojure.core/update-in`, in the sense that
  the event handler is given a specific part of `app-db` to change, not all of `app-db`.

  The interceptor has both a `:before` and `:after` functions. The `:before` replaces
  the `:db` key within coeffects with a sub-path within `app-db`. The `:after` reverses the process,
  and it grafts the handler's return value back into db, at the right path.

  Examples:

      #!clj
      (path :some :path)
      (path [:some :path])
      (path [:some :path] :to :here)
      (path [:some :path] [:to] :here)

  Example Use:

      #!clj
      (reg-event-db
        :event-id
        (path [:a :b])  ;; <-- used here, in interceptor chain
        (fn [b v]       ;; 1st arg is not db. Is the value from path [:a :b] within db
          ... new-b))   ;; returns a new value for that path (not the entire db)

  Notes:

    1. `path` may appear more than once in an interceptor chain. Progressive narrowing.
    2. if `:effects` contains no `:db` effect, can't graft a value back in.
  "
  {:api-docs/heading "Interceptors"}
  [& args]
  (apply std-interceptors/path args))

(defn enrich
  "Returns an interceptor which will run the given function `f` in the `:after`
  position.

  `f` is called with two arguments: `db` and `event`, and is expected to
  return a modified `db`.

  Unlike the `after` interceptor which is only about side effects, `enrich`
  expects `f` to process and alter the given `db` coeffect in some useful way,
  contributing to the derived data, flowing vibe.

  If `f` returns `nil`, the `db` value passed to `f` will be returned instead.

  #### Example Use:

  Imagine that todomvc needed to do duplicate detection - if any two todos had
  the same text, then highlight their background, and report them via a warning
  at the bottom of the panel.

  Almost any user action (edit text, add new todo, remove a todo) requires a
  complete reassessment of duplication errors and warnings. E.g. that edit
  just made might have introduced a new duplicate, or removed one. Same with
  any todo removal. So we need to re-calculate warnings after any CRUD events
  associated with the todos list.

  Unless we are careful, we might end up coding subtly different checks
  for each kind of CRUD operation.  The duplicates check made after
  'delete todo' event might be subtly different to that done after an
  editing operation. Nice and efficient, but fiddly. A bug generator
  approach.

  So, instead, we create an `f` which recalculates ALL warnings from scratch
  every time there is ANY change. It will inspect all the todos, and
  reset ALL FLAGS every time (overwriting what was there previously)
  and fully recalculate the list of duplicates (displayed at the bottom?).

  <a href=\"https://twitter.com/nathanmarz/status/879722740776939520\" target=\"_blank\">https://twitter.com/nathanmarz/status/879722740776939520</a>

  By applying `f` in an `:enrich` interceptor, after every CRUD event,
  we keep the handlers simple and yet we ensure this important step
  (of getting warnings right) is not missed on any change.

  We can test `f` easily - it is a pure function - independently of
  any CRUD operation.

  This brings huge simplicity at the expense of some re-computation
  each time. This may be a very satisfactory trade-off in many cases.

  #### Returning nil

  In some cases, it's useful to apply a change to specific situations that can
  be determined at runtime instead of when defining the handler with an
  `:enrich` interceptor. Instead of forcing you to return the `db` from every
  non-applicable branch, you can return `nil` to use the given `db` value:

      #!clj
      (def set-last-update
        (core/enrich
          (fn [{db :db} [_ {user :user}]]
            (when (active-user? user)  ;; <- Only perform an update if user is active
              ...))))
  "
  {:api-docs/heading "Interceptors"}
  [f]
  (std-interceptors/enrich f))

(def ^{:api-docs/heading "Interceptors"} unwrap
  "> New in v1.2.0

   An interceptor which decreases the amount of destructuring necessary in an
   event handler where the event is structured as a 2-vector of
   [event-id payload-map].

   It promotes the `payload-map` part to be the event ultimately given to the
   event handler. Should you want the full original event, it can be found in
   `coeffects` under the key `:original-event`.

   If a dispatch looked like this:

      #!clj
       (dispatch [:event-id {:x 1 :y 2 :z 3}])

   Your event handlers can look like this:

      #!clj
       (reg-event-fx
         :event-id
         [... unwrap ...]                    ;; <-- added to the interceptors
         (fn [{:keys [db]} {:keys [x y z]}]  ;; <-- instead of [_ {:keys [x y z]}]
           ...)
   "
  std-interceptors/unwrap)

(def ^{:api-docs/heading "Interceptors"} trim-v
  "An interceptor which removes the first element of the event vector,
  before it is supplied to the event handler, allowing you to write more
   aesthetically pleasing event handlers. No leading underscore on the event-v!

  Should you want the full original event, it can be found in `coeffects` under
  the key `:original-event`.

  Your event handlers will look like this:

      #!clj
      (reg-event-db
        :event-id
        [... trim-v ...]    ;; <-- added to the interceptors
        (fn [db [x y z]]    ;; <-- instead of [_ x y z]
          ...)
    "
  std-interceptors/trim-v)

(defn after
  "Returns an interceptor which runs the given function `f` in the `:after`
  position, presumably for side effects.

  `f` is called with two arguments: the `:effects` value for `:db`
  (or the `:coeffect` value of `:db` if no `:db` effect is returned) and the event.
  Its return value is ignored, so `f` can only side-effect.

  An example of use can be seen in the re-frame github repo in `/examples/todomvc/events.cljs`:

     - `f` runs schema validation (reporting any errors found).
     - `f` writes to localstorage."
  {:api-docs/heading "Interceptors"}
  [f]
  (std-interceptors/after f))

(defn on-changes
  "Returns an interceptor which will observe N paths within `db`, and if any of them
  test not `identical?` to their previous value  (as a result of a event handler
  being run), then it will run `f` to compute a new value, which is then assoc-ed
  into the given `out-path` within `db`.

  Example Usage:

      #!clj
      (defn my-f
        [a-val b-val]
        ... some computation on a and b in here)

      ;; use it
      (def my-interceptor (on-changes my-f [:c] [:a] [:b]))

      (reg-event-db
        :event-id
        [... my-interceptor ...]  ;; <-- ultimately used here
        (fn [db v]
           ...))


  If you put this interceptor on handlers which might change paths `:a` or `:b`,
  it will:

    - call `f` each time the value at path `[:a]` or `[:b]` changes
    - call `f` with the values extracted from `[:a]` `[:b]`
    - assoc the return value from `f` into the path  `[:c]`
  "
  {:api-docs/heading "Interceptors"}
  [f out-path & in-paths]
  (apply std-interceptors/on-changes f out-path in-paths))

(defn reg-global-interceptor
  "Registers the given `interceptor` as a global interceptor. Global interceptors are
   included in the processing chain of every event.

   When you register an event handler, you have the option of supplying an
   interceptor chain. Any global interceptors you register are effectively
   prepending to this chain.

   Global interceptors are run in the order that they are registered.

   Global interceptors are unique by :id. If a global interceptor with the same :id
   key as `interceptor` is already registered, `interceptor` will take its place in the
   global interceptor chain. This facilitates hot-reloading.

   Note: members of re-frame.std-interceptors do not have unique ids. To register
   more than one, consider:

  (reg-global-interceptor (-> (re-frame.std-interceptors/on-changes + [:a] [:b])
                              (assoc :id :my-unique-id)))"
  {:api-docs/heading "Global Interceptors"}
  [interceptor]
  (settings/reg-global-interceptor interceptor))

(defn clear-global-interceptor
  "Unregisters global interceptors (presumably registered previously via the use of `reg-global-interceptor`).

  When called with no args, it will unregister all currently registered global interceptors.

  When given one arg, assumed to be the `id` of a previously registered
  global interceptors, it will unregister the associated interceptor. Will produce a warning to
  console if it finds no matching registration."
  {:api-docs/heading "Global Interceptors"}
  ([]
   (settings/clear-global-interceptors))
  ([id]
   (settings/clear-global-interceptors id)))

(defn ->interceptor
  "A utility function for creating interceptors.

  Accepts three optional, named arguments:

     - `:id` - an id for the interceptor (decorative only)
     - `:before` - the interceptor's before function
     - `:after`  - the interceptor's after function

  Example use:

      #!clj
      (def my-interceptor
        (->interceptor
         :id     :my-interceptor
         :before (fn [context]
                   ... modifies and returns `context`)
         :after  (fn [context]
                   ... modifies and returns `context`)))

  Notes:

    - `:before` functions modify and return their `context` argument. Sometimes they
      only side effect, in which case, they'll perform the side effect and return
      `context` unchanged.
    - `:before` functions often modify the `:coeffects` map within `context` and,
      if they do, then they should use the utility functions `get-coeffect` and
      `assoc-coeffect`.
    - `:after` functions modify and return their `context` argument. Sometimes they
      only side effect, in which case, they'll perform the side effect and return
      `context` unchanged.
    - `:after` functions often modify the `:effects` map within `context` and,
      if they do, then they should use the utility functions `get-effect`
      and `assoc-effect`"
  {:api-docs/heading "Writing Interceptors"}
  [& {:as m :keys [id before after]}]
  (utils/apply-kw interceptor/->interceptor m))

(defn get-coeffect
  "A utility function, typically used when writing an interceptor's `:before` function.

   When called with one argument, it returns the `:coeffects` map from within that `context`.

   When called with two or three arguments, behaves like `clojure.core/get` and
   returns the value mapped to `key` in the `:coeffects` map within `context`, `not-found` or
   `nil` if `key` is not present."
  {:api-docs/heading "Writing Interceptors"}
  ([context]
   (interceptor/get-coeffect context))
  ([context key]
   (interceptor/get-coeffect context key))
  ([context key not-found]
   (interceptor/get-coeffect context key not-found)))

(defn assoc-coeffect
  "A utility function, typically used when writing an interceptor's `:before` function.

   Adds or updates a key/value pair in the `:coeffects` map within `context`. "
  {:api-docs/heading "Writing Interceptors"}
  [context key value]
  (interceptor/assoc-coeffect context key value))

(defn get-effect
  "A utility function, used when writing interceptors, typically within an `:after` function.

   When called with one argument, returns the `:effects` map from the `context`.

   When called with two or three arguments, behaves like `clojure.core/get` and
   returns the value mapped to `key` in the effects map, `not-found` or
   `nil` if `key` is not present."
  {:api-docs/heading "Writing Interceptors"}
  ([context]
   (interceptor/get-effect context))
  ([context key]
   (interceptor/get-effect context key))
  ([context key not-found]
   (interceptor/get-effect context key not-found)))

(defn assoc-effect
  "A utility function, typically used when writing an interceptor's `:after` function.

   Adds or updates a key/value pair in the `:effects` map within `context`. "
  {:api-docs/heading "Writing Interceptors"}
  [context key value]
  (interceptor/assoc-effect context key value))

(defn enqueue
  "A utility function, used when writing an interceptor's `:before` function.

  Adds the given collection of `interceptors` to those already in `context's`
  execution `:queue`. It returns the updated `context`.

  So, it provides a way for one interceptor to add more interceptors to the
  currently executing interceptor chain.
  "
  {:api-docs/heading "Writing Interceptors"}
  [context interceptors]
  (interceptor/enqueue context interceptors))

;; --  logging ----------------------------------------------------------------

(defn set-loggers!
  "re-frame outputs warnings and errors via the API function `console`
   which, by default, delegates to `js/console`'s default implementation for
  `log`, `error`, `warn`, `debug`, `group` and `groupEnd`. But, using this function,
   you can override that behaviour with your own functions.

  The argument `new-loggers` should be a map containing a subset of they keys
  for the standard `loggers`, namely  `:log` `:error` `:warn` `:debug` `:group`
  or `:groupEnd`.

  Example Usage:

      #!clj
      (defn my-logger      ;; my alternative logging function
        [& args]
        (post-it-somewhere (apply str args)))

      ;; now install my alternative loggers
      (re-frame.core/set-loggers!  {:warn my-logger :log my-logger})
   "
  {:api-docs/heading "Logging"}
  [new-loggers]
  (loggers/set-loggers! new-loggers))

(defn console
  "A utility logging function which is used internally within re-frame to produce
  warnings and other output. It can also be used by libraries which
  extend re-frame, such as effect handlers.

  By default, it will output the given `args` to `js/console` at the given log `level`.
  However, an application using re-frame can redirect `console` output via `set-loggers!`.

  `level` can be one of `:log`, `:error`, `:warn`, `:debug`, `:group` or `:groupEnd`.

  Example usage:

      #!clj
      (console :error \"Sure enough it happened:\" a-var \"and\" another)
      (console :warn \"Possible breach of containment wall at:\" dt)
  "
  {:api-docs/heading "Logging"}
  [level & args]
  (apply loggers/console level args))

;; -- unit testing ------------------------------------------------------------

(defn make-restore-fn
  "This is a utility function, typically used in testing.

  It checkpoints the current state of re-frame and returns a function which, when
  later called, will restore re-frame to the checkpointed state.

  The checkpoint includes `app-db`, all registered handlers and all subscriptions.
  "
  {:api-docs/heading "Miscellaneous"}
  []
  (let [handlers @registrar/kind->id->handler
        app-db   @db/app-db
        subs-cache @subs/query->reaction]
    (fn []
      ;; call `dispose!` on all current subscriptions which
      ;; didn't originally exist.
      (let [original-subs (set (vals subs-cache))
            current-subs  (set (vals @subs/query->reaction))]
        (doseq [sub (set/difference current-subs original-subs)]
          (interop/dispose! sub)))

      ;; Reset the atoms
      ;; We don't need to reset subs/query->reaction, as
      ;; disposing of the subs removes them from the cache anyway
      (reset! registrar/kind->id->handler handlers)
      (reset! db/app-db app-db)
      nil)))

(defn purge-event-queue
  "Removes all events currently queued for processing"
  {:api-docs/heading "Miscellaneous"}
  []
  (router/purge re-frame.router/event-queue))

;; -- Event Processing Callbacks  ---------------------------------------------

(defn add-post-event-callback
  "Registers the given function `f` to be called after each event is processed.

   `f` will be called with two arguments:

    - `event`: a vector. The event just processed.
    - `queue`: a PersistentQueue, possibly empty, of events yet to be processed.

   This facility is useful in advanced cases like:

     - you are implementing a complex bootstrap pipeline
     - you want to create your own handling infrastructure, with perhaps multiple
       handlers for the one event, etc.  Hook in here.
     - libraries providing 'isomorphic javascript' rendering on  Nodejs or Nashorn.

  `id` is typically a keyword. If it supplied when an `f` is added, it can be
  subsequently be used to identify it for removal. See `remove-post-event-callback`.
  "
  {:api-docs/heading "Miscellaneous"}
  ([f]
   (add-post-event-callback f f))   ;; use f as its own identifier
  ([id f]
   (router/add-post-event-callback re-frame.router/event-queue id f)))

(defn remove-post-event-callback
  "Unregisters a post event callback function, identified by `id`.

  Such a function must have been previously registered via `add-post-event-callback`"
  {:api-docs/heading "Miscellaneous"}
  [id]
  (router/remove-post-event-callback re-frame.router/event-queue id))

;; --  Deprecation ------------------------------------------------------------
;; Assisting the v0.7.x ->  v0.8.x transition.
(defn register-handler
  "Deprecated. Use `reg-event-db` instead."
  {:deprecated "0.8.0"
   :api-docs/heading "Deprecated"}
  [& args]
  (console :warn  "re-frame: \"register-handler\" has been renamed \"reg-event-db\" (look for registration of " (str (first args)) ")")
  (apply reg-event-db args))

(defn register-sub
  "Deprecated. Use `reg-sub-raw` instead."
  {:deprecated "0.8.0"
   :api-docs/heading "Deprecated"}
  [& args]
  (console :warn  "re-frame: \"register-sub\" is used to register the event " (str (first args)) " but it is a deprecated part of the API. Please use \"reg-sub-raw\" instead.")
  (apply reg-sub-raw args))
````

## File: src/re_frame/db.cljc
````
(ns re-frame.db
  (:require [re-frame.interop :refer [ratom]]))

;; -- Application State  --------------------------------------------------------------------------
;;
;; Should not be accessed directly by application code.
;; Read access goes through subscriptions.
;; Updates via event handlers.
(def app-db (ratom {}))
````

## File: src/re_frame/events.cljc
````
(ns re-frame.events
  (:require [re-frame.db          :refer [app-db]]
            [re-frame.utils       :refer [first-in-vector]]
            [re-frame.interop     :refer [empty-queue debug-enabled?]]
            [re-frame.registrar   :refer [get-handler register-handler]]
            [re-frame.loggers     :refer [console]]
            [re-frame.interceptor :as  interceptor]
            [re-frame.trace       :as trace :include-macros true]))

(def kind :event)
(assert (re-frame.registrar/kinds kind))

(defn- flatten-and-remove-nils
  "`interceptors` might have nested collections, and contain nil elements.
  return a flat collection, with all nils removed.
  This function is 9/10 about giving good error messages."
  [id interceptors]
  (let [make-chain  #(->> % flatten (remove nil?))]
    (if-not debug-enabled?
      (make-chain interceptors)
      (do    ;; do a whole lot of development time checks
        (when-not (coll? interceptors)
          (console :error "re-frame: when registering" id ", expected a collection of interceptors, got:" interceptors))
        (let [chain (make-chain interceptors)]
          (when (empty? chain)
            (console :error "re-frame: when registering" id ", given an empty interceptor chain"))
          (when-let [not-i (first (remove interceptor/interceptor? chain))]
            (if (fn? not-i)
              (console :error "re-frame: when registering" id ", got a function instead of an interceptor. Did you provide old style middleware by mistake? Got:" not-i)
              (console :error "re-frame: when registering" id ", expected interceptors, but got:" not-i)))
          chain)))))

(defn register
  "Associate the given event `id` with the given collection of `interceptors`.

   `interceptors` may contain nested collections and there may be nils
   at any level,so process this structure into a simple, nil-less vector
   before registration.

   Typically, an `event handler` will be at the end of the chain (wrapped
   in an interceptor)."
  [id interceptors]
  (register-handler kind id (flatten-and-remove-nils id interceptors)))

;; -- handle event --------------------------------------------------------------------------------

(def ^:dynamic *handling* nil)    ;; remember what event we are currently handling

(defn handle
  "Given an event vector `event-v`, look up the associated interceptor chain, and execute it."
  [event-v]
  (let [event-id  (first-in-vector event-v)]
    (if-let [interceptors  (get-handler kind event-id true)]
      (if *handling*
        (console :error "re-frame: while handling" *handling* ", dispatch-sync was called for" event-v ". You can't call dispatch-sync within an event handler.")
        (binding [*handling*  event-v]
          (trace/with-trace {:operation event-id
                             :op-type   kind
                             :tags      {:event event-v}}
            (trace/merge-trace! {:tags {:app-db-before @app-db}})
            (interceptor/execute event-v interceptors)
            (trace/merge-trace! {:tags {:app-db-after @app-db}})))))))
````

## File: src/re_frame/fx.cljc
````
(ns re-frame.fx
  (:require
   [re-frame.router      :as router]
   [re-frame.db          :refer [app-db]]
   [re-frame.interceptor :refer [->interceptor]]
   [re-frame.interop     :refer [set-timeout!]]
   [re-frame.events      :as events]
   [re-frame.registrar   :refer [get-handler clear-handlers register-handler]]
   [re-frame.loggers     :refer [console]]
   [re-frame.trace :as trace :include-macros true]))

;; -- Registration ------------------------------------------------------------

(def kind :fx)
(assert (re-frame.registrar/kinds kind))

(defn reg-fx
  [id handler]
  (register-handler kind id handler))

;; -- Interceptor -------------------------------------------------------------

(def do-fx
  "An interceptor whose `:after` actions the contents of `:effects`. As a result,
  this interceptor is Domino 3.

  This interceptor is silently added (by reg-event-db etc) to the front of
  interceptor chains for all events.

  For each key in `:effects` (a map), it calls the registered `effects handler`
  (see `reg-fx` for registration of effect handlers).

  So, if `:effects` was:
      {:dispatch  [:hello 42]
       :db        {...}
       :undo      \"set flag\"}

  it will call the registered effect handlers for each of the map's keys:
  `:dispatch`, `:undo` and `:db`. When calling each handler, provides the map
  value for that key - so in the example above the effect handler for :dispatch
  will be given one arg `[:hello 42]`.

  You cannot rely on the ordering in which effects are executed, other than that
  `:db` is guaranteed to be executed first."
  (->interceptor
   :id :do-fx
   :after (fn do-fx-after
            [context]
            (trace/with-trace
              {:op-type :event/do-fx}
              (let [effects            (:effects context)
                    effects-without-db (dissoc effects :db)]
                 ;; :db effect is guaranteed to be handled before all other effects.
                (when-let [new-db (:db effects)]
                  ((get-handler kind :db false) new-db))
                (doseq [[effect-key effect-value] effects-without-db]
                  (if-let [effect-fn (get-handler kind effect-key false)]
                    (effect-fn effect-value)
                    (console :warn
                             "re-frame: no handler registered for effect:"
                             effect-key
                             ". Ignoring."
                             (when (= :event effect-key)
                               (str "You may be trying to return a coeffect map from an event-fx handler. "
                                    "See https://day8.github.io/re-frame/use-cofx-as-fx/"))))))))))

;; -- Builtin Effect Handlers  ------------------------------------------------

;; :dispatch-later
;;
;; `dispatch` one or more events after given delays. Expects a collection
;; of maps with two keys:  :`ms` and `:dispatch`
;;
;; usage:
;;
;;    {:dispatch-later [{:ms 200 :dispatch [:event-id "param"]}    ;;  in 200ms do this: (dispatch [:event-id "param"])
;;                      {:ms 100 :dispatch [:also :this :in :100ms]}]}
;;
;; Note: nil entries in the collection are ignored which means events can be added
;; conditionally:
;;    {:dispatch-later [ (when (> 3 5) {:ms 200 :dispatch [:conditioned-out]})
;;                       {:ms 100 :dispatch [:another-one]}]}
;;
(defn dispatch-later
  [{:keys [ms dispatch] :as effect}]
  (if (or (empty? dispatch) (not (number? ms)))
    (console :error "re-frame: ignoring bad :dispatch-later value:" effect)
    (set-timeout! #(router/dispatch dispatch) ms)))

(reg-fx
 :dispatch-later
 (fn [value]
   (if (map? value)
     (dispatch-later value)
     (doseq [effect (remove nil? value)]
       (dispatch-later effect)))))

;; :fx
;;
;; Handle one or more effects. Expects a collection of vectors (tuples) of the
;; form [effect-key effect-value]. `nil` entries in the collection are ignored
;; so effects can be added conditionally.
;;
;; usage:
;;
;; {:fx [[:dispatch [:event-id "param"]]
;;       nil
;;       [:http-xhrio {:method :post
;;                     ...}]]}
;;

(reg-fx
 :fx
 (fn [seq-of-effects]
   (if-not (sequential? seq-of-effects)
     (console :warn "re-frame: \":fx\" effect expects a seq, but was given " (type seq-of-effects))
     (doseq [[effect-key effect-value] (remove nil? seq-of-effects)]
       (when (= :db effect-key)
         (console :warn "re-frame: \":fx\" effect should not contain a :db effect"))
       (if-let [effect-fn (get-handler kind effect-key false)]
         (effect-fn effect-value)
         (console :warn "re-frame: in \":fx\" effect found " effect-key " which has no associated handler. Ignoring."))))))

;; :dispatch
;;
;; `dispatch` one event. Expects a single vector.
;;
;; usage:
;;   {:dispatch [:event-id "param"] }

(reg-fx
 :dispatch
 (fn [value]
   (if-not (vector? value)
     (console :error "re-frame: ignoring bad :dispatch value. Expected a vector, but got:" value)
     (router/dispatch value))))

;; :dispatch-n
;;
;; `dispatch` more than one event. Expects a list or vector of events. Something for which
;; sequential? returns true.
;;
;; usage:
;;   {:dispatch-n (list [:do :all] [:three :of] [:these])}
;;
;; Note: nil events are ignored which means events can be added
;; conditionally:
;;    {:dispatch-n (list (when (> 3 5) [:conditioned-out])
;;                       [:another-one])}
;;
(reg-fx
 :dispatch-n
 (fn [value]
   (if-not (sequential? value)
     (console :error "re-frame: ignoring bad :dispatch-n value. Expected a collection, but got:" value)
     (doseq [event (remove nil? value)] (router/dispatch event)))))

;; :deregister-event-handler
;;
;; removes a previously registered event handler. Expects either a single id (
;; typically a namespaced keyword), or a seq of ids.
;;
;; usage:
;;   {:deregister-event-handler :my-id)}
;; or:
;;   {:deregister-event-handler [:one-id :another-id]}
;;
(reg-fx
 :deregister-event-handler
 (fn [value]
   (let [clear-event (partial clear-handlers events/kind)]
     (if (sequential? value)
       (doseq [event value] (clear-event event))
       (clear-event value)))))

;; :db
;;
;; reset! app-db with a new value. `value` is expected to be a map.
;;
;; usage:
;;   {:db  {:key1 value1 key2 value2}}
;;
(reg-fx
 :db
 (fn [value]
   (if-not (identical? @app-db value)
     (reset! app-db value)
     (trace/with-trace {:op-type :reagent/quiescent}))))
````

## File: src/re_frame/interceptor.cljc
````
(ns re-frame.interceptor
  (:require
   [re-frame.loggers :refer [console]]
   [re-frame.interop :refer [empty-queue debug-enabled?]]
   [re-frame.trace :as trace :include-macros true]
   [re-frame.registrar :as registrar]
   [re-frame.utils :as u]
   [clojure.set :as set]))

(def mandatory-interceptor-keys #{:id :after :before})

(def optional-interceptor-keys #{:comment})

(defn interceptor?
  [m]
  (and (map? m)
       (= mandatory-interceptor-keys
          (-> m keys set (set/difference optional-interceptor-keys)))))

(defn ->interceptor
  [& {:as m :keys [id comment before after]}]
  (when debug-enabled?
    (if-let [unknown-keys (seq (set/difference
                                (-> m keys set)
                                mandatory-interceptor-keys
                                optional-interceptor-keys))]
      (console :error "re-frame: ->interceptor" m "has unknown keys:" unknown-keys)))
  (cond-> {:id     (or id :unnamed)
           :before before
           :after  after}
    comment (assoc :comment comment)))

;; -- Effect Helpers  -----------------------------------------------------------------------------

(defn get-effect
  ([context]
   (:effects context))
  ([context key]
   (get-in context [:effects key]))
  ([context key not-found]
   (get-in context [:effects key] not-found)))

(defn assoc-effect
  [context key value]
  (assoc-in context [:effects key] value))

(defn update-effect
  [context key f & args]
  (apply update-in context [:effects key] f args))

;; -- CoEffect Helpers  ---------------------------------------------------------------------------

(defn get-coeffect
  ([context]
   (:coeffects context))
  ([context key]
   (get-in context [:coeffects key]))
  ([context key not-found]
   (get-in context [:coeffects key] not-found)))

(defn assoc-coeffect
  [context key value]
  (assoc-in context [:coeffects key] value))

(defn update-coeffect
  [context key f & args]
  (apply update-in context [:coeffects key] f args))

;; -- Execute Interceptor Chain  ------------------------------------------------------------------

(defn- exception->ex-info [e interceptor direction]
  (ex-info (str "Interceptor Exception: " #?(:clj (.getMessage e) :cljs (ex-message e)))
           {:direction direction
            :interceptor (:id interceptor)}
           e))

(defn- invoke-interceptor-fn
  [{::keys [original-exception?] :as context} interceptor direction]
  (let [f (get interceptor direction)]
    (cond
      (not f) context
      original-exception? (f context)
      :else
      (try
        (f context)
        (catch #?(:clj Exception :cljs :default) e
          (throw (exception->ex-info e interceptor direction)))))))

(defn- invoke-interceptors
  "Loop over all interceptors, calling `direction` function on each,
  threading the value of `context` through every call.

  `direction` is one of `:before` or `:after`.

  Each iteration, the next interceptor to process is obtained from
  context's `:queue`. After they are processed, interceptors are popped
  from `:queue` and added to `:stack`.

  After sufficient iteration, `:queue` will be empty, and `:stack` will
  contain all interceptors processed.

  Returns updated `context`. Ie. the `context` which has been threaded
  through all interceptor functions.

  Generally speaking, an interceptor's `:before` function will (if present)
  add to a `context's` `:coeffects`, while its `:after` function
  will modify the `context`'s `:effects`.  Very approximately.

  But because all interceptor functions are given `context`, and can
  return a modified version of it, the way is clear for an interceptor
  to introspect the stack or queue, or even modify the queue
  (add new interceptors via `enqueue`?). This is a very fluid arrangement."
  ([context direction]
   (loop [context context]
     (let [queue (:queue context)]        ;; future interceptors
       (if (empty? queue)
         context
         (let [interceptor (peek queue)   ;; next interceptor to call
               stack (:stack context)]    ;; already completed interceptors
           (recur (-> context
                      (assoc :queue (pop queue)
                             :stack (conj stack interceptor))
                      (invoke-interceptor-fn interceptor direction)))))))))

(defn enqueue
  [context interceptors]
  (update context :queue
          (fnil into empty-queue)
          interceptors))

(defn- context
  "Create a fresh context"
  ([event interceptors]
   (-> {}
       (assoc-coeffect :event event)
      ;; Some interceptors, like `trim-v` and `unwrap`, alter event so capture
      ;; the original for use cases such as tracing.
       (assoc-coeffect :original-event event)
       (enqueue interceptors)))
  ([event interceptors db]      ;; only used in tests, probably a hack, remove ?  XXX
   (-> (context event interceptors)
       (assoc-coeffect :db db))))

(defn- change-direction
  "Called on completion of `:before` processing, this function prepares/modifies
   `context` for the backwards sweep of processing in which an interceptor
   chain's `:after` fns are called.

  At this point in processing, the `:queue` is empty and `:stack` holds all
  the previously run interceptors. So this function enables the backwards walk
  by priming `:queue` with what's currently in `:stack`"
  [context]
  (-> context
      (dissoc :queue)
      (enqueue (:stack context))))

(defn execute*
  [ctx]
  (-> ctx
      (invoke-interceptors :before)
      change-direction
      (invoke-interceptors :after)))

(defn- merge-ex-data [e & ms]
  (ex-info #?(:clj (.getMessage e) :cljs (ex-message e))
           (apply merge (ex-data e) ms)
           #?(:clj (.getCause e) :cljs (ex-cause e))))

(defn default-error-handler [original-error re-frame-error]
  (let [{:keys [event-v direction interceptor]} (ex-data re-frame-error)
        event-handler? (#{:db-handler :fx-handler :ctx-handler} interceptor)]
    (apply console :error
           "An error occurred while handling the re-frame event:"
           (str event-v)
           "\n"
           (map str
                (if event-handler?
                  ["Within the" (first event-v) "event handler function."]
                  ["Within the" direction "phase of the" (pr-str interceptor) "interceptor."])))
    (throw original-error)))

(defn execute
  "Executes the given chain (coll) of interceptors.

   Each interceptor has this form:
       {:before  (fn [context] ...)     ;; returns possibly modified context
        :after   (fn [context] ...)}    ;; `identity` would be a noop

   Walks the queue of interceptors from beginning to end, calling the
   `:before` fn on each, then reverse direction and walk backwards,
   calling the `:after` fn on each.

   The last interceptor in the chain presumably wraps an event
   handler fn. So the overall goal of the process is to \"handle
   the given event\".

   Thread a `context` through all calls. `context` has this form:

     {:coeffects {:event [:a-query-id :some-param]
                  :db    <original contents of app-db>}
      :effects   {:db    <new value for app-db>
                  :fx  [:dispatch [:an-event-id :param1]]}
      :queue     <a collection of further interceptors>
      :stack     <a collection of interceptors already walked>}

   `context` has `:coeffects` and `:effects` which, if this was a web
   server, would be somewhat analogous to `request` and `response`
   respectively.

   `coeffects` will contain data like `event` and the initial
   state of `db` -  the inputs required by the event handler
   (sitting presumably on the end of the chain), while handler-returned
   side effects are put into `:effects` including, but not limited to,
   new values for `db`.

   The first few interceptors in a chain will likely have `:before`
   functions which \"prime\" the `context` by adding the event, and
   the current state of app-db into `:coeffects`. But interceptors can
   add whatever they want to `:coeffects` - perhaps the event handler needs
   some information from localstore, or a random number, or access to
   a DataScript connection.

   Equally, some interceptors in the chain will have `:after` fn
   which can process the side effects accumulated into `:effects`
   including but, not limited to, updates to app-db.

   Through both stages (before and after), `context` contains a `:queue`
   of interceptors yet to be processed, and a `:stack` of interceptors
   already done.  In advanced cases, these values can be modified by the
   functions through which the context is threaded."
  [event-v interceptors]
  (let [ctx (context event-v interceptors)
        error-handler (registrar/get-handler :error :event-handler)]
    (trace/merge-trace!
     {:tags {:interceptors interceptors}})
    (if-not error-handler
      (execute* (assoc ctx ::original-exception? true))
      (try
        (execute* ctx)
        (catch #?(:clj Exception :cljs :default) e
          (error-handler (ex-cause e)
                         (merge-ex-data e {:event-v event-v})))))))
````

## File: src/re_frame/subs.cljc
````
(ns re-frame.subs
  (:require
   [re-frame.db        :refer [app-db]]
   [re-frame.interop   :refer [add-on-dispose! debug-enabled? make-reaction ratom? deref? dispose! reagent-id reactive?]]
   [re-frame.loggers   :refer [console]]
   [re-frame.utils     :refer [first-in-vector]]
   [re-frame.registrar :refer [get-handler clear-handlers register-handler]]
   [re-frame.trace     :as trace :include-macros true]))

(def kind :sub)
(assert (re-frame.registrar/kinds kind))

;; -- cache -------------------------------------------------------------------
;;
;; De-duplicate subscriptions. If two or more equal subscriptions
;; are concurrently active, we want only one handler running.
;; Two subscriptions are "equal" if their query vectors test "=".
(def query->reaction (atom {}))

(defn clear-subscription-cache!
  "calls `on-dispose` for each cached item,
   which will cause the value to be removed from the cache"
  []
  (doseq [[k rxn] @query->reaction]
    (dispose! rxn))
  (if (not-empty @query->reaction)
    (console :warn "re-frame: The subscription cache isn't empty after being cleared")))

(defn clear-all-handlers!
  "Unregisters all existing subscription handlers"
  []
  (clear-handlers kind)
  (clear-subscription-cache!))

(defn cache-and-return
  "cache the reaction r"
  [query-v dynv r]
  (let [cache-key [query-v dynv]]
    ;; when this reaction is no longer being used, remove it from the cache
    (add-on-dispose! r #(trace/with-trace {:operation (first-in-vector query-v)
                                           :op-type   :sub/dispose
                                           :tags      {:query-v  query-v
                                                       :reaction (reagent-id r)}}
                          (swap! query->reaction
                                 (fn [query-cache]
                                   (if (and (contains? query-cache cache-key) (identical? r (get query-cache cache-key)))
                                     (dissoc query-cache cache-key)
                                     query-cache)))))
    ;; cache this reaction, so it can be used to deduplicate other, later "=" subscriptions
    (swap! query->reaction (fn [query-cache]
                             (when debug-enabled?
                               (when (contains? query-cache cache-key)
                                 (console :warn "re-frame: Adding a new subscription to the cache while there is an existing subscription in the cache" cache-key)))
                             (assoc query-cache cache-key r)))
    (trace/merge-trace! {:tags {:reaction (reagent-id r)}})
    r)) ;; return the actual reaction

(defn cache-lookup
  ([query-v]
   (cache-lookup query-v []))
  ([query-v dyn-v]
   (get @query->reaction [query-v dyn-v])))

;; -- subscribe ---------------------------------------------------------------

(defn warn-when-not-reactive
  []
  (when (and debug-enabled? (not (reactive?)))
    (console :warn
             "re-frame: Subscribe was called outside of a reactive context.\n"
             "https://day8.github.io/re-frame/FAQs/UseASubscriptionInAnEventHandler/")))

(defn subscribe
  ([query]
   (warn-when-not-reactive)
   (trace/with-trace {:operation (first-in-vector query)
                      :op-type   :sub/create
                      :tags      {:query-v query}}
     (if-let [cached (cache-lookup query)]
       (do
         (trace/merge-trace! {:tags {:cached?  true
                                     :reaction (reagent-id cached)}})
         cached)

       (let [query-id   (first-in-vector query)
             handler-fn (get-handler kind query-id)]
         (trace/merge-trace! {:tags {:cached? false}})
         (if (nil? handler-fn)
           (do (trace/merge-trace! {:error true})
               (console :error (str "re-frame: no subscription handler registered for: " query-id ". Returning a nil subscription.")))
           (cache-and-return query [] (handler-fn app-db query)))))))

  ([query dynv]
   (warn-when-not-reactive)
   (trace/with-trace {:operation (first-in-vector query)
                      :op-type   :sub/create
                      :tags      {:query-v query
                                  :dyn-v   dynv}}
     (if-let [cached (cache-lookup query dynv)]
       (do
         (trace/merge-trace! {:tags {:cached?  true
                                     :reaction (reagent-id cached)}})
         cached)
       (let [query-id   (first-in-vector query)
             handler-fn (get-handler kind query-id)]
         (trace/merge-trace! {:tags {:cached? false}})
         (when debug-enabled?
           (when-let [not-reactive (not-empty (remove ratom? dynv))]
             (console :warn "re-frame: your subscription's dynamic parameters that don't implement IReactiveAtom:" not-reactive)))
         (if (nil? handler-fn)
           (do (trace/merge-trace! {:error true})
               (console :error (str "re-frame: no subscription handler registered for: " query-id ". Returning a nil subscription.")))
           (let [dyn-vals (make-reaction (fn [] (mapv deref dynv)))
                 sub      (make-reaction (fn [] (handler-fn app-db query @dyn-vals)))]
             ;; handler-fn returns a reaction which is then wrapped in the sub reaction
             ;; need to double deref it to get to the actual value.
             ;(console :log "Subscription created: " v dynv)
             (cache-and-return query dynv (make-reaction (fn [] @@sub))))))))))

;; -- reg-sub -----------------------------------------------------------------

(defn- map-vals
  "Returns a new version of 'm' in which 'f' has been applied to each value.
  (map-vals inc {:a 4, :b 2}) => {:a 5, :b 3}"
  [f m]
  (into (empty m)
        (map (fn [[k v]] [k (f v)]))
        m))

(defn map-signals
  "Runs f over signals. Signals may take several
  forms, this function handles all of them."
  [f signals]
  (cond
    (sequential? signals) (map f signals)
    (map? signals) (map-vals f signals)
    (deref? signals) (f signals)
    :else '()))

(defn to-seq
  "Coerces x to a seq if it isn't one already"
  [x]
  (if (sequential? x)
    x
    (list x)))

(defn deref-input-signals
  [signals query-id]
  (let [dereffed-signals (map-signals deref signals)]
    (cond
      (sequential? signals) (map deref signals)
      (map? signals) (map-vals deref signals)
      (deref? signals) (deref signals)
      :else (console :error "re-frame: in the reg-sub for" query-id ", the input-signals function returns:" signals))
    (trace/merge-trace! {:tags {:input-signals (doall (to-seq (map-signals reagent-id signals)))}})
    dereffed-signals))

(defn sugar [query-id sub-fn query? & args]
  (let [error-header (str "re-frame: reg-sub for " query-id ", ")
        [op f :as comp-f] (take-last 2 args)
        [input-args      ;; may be empty, or one signal fn, or pairs of  :<- / vector
         computation-fn] (if (or (= 1 (count comp-f))
                                 (fn? op)
                                 (query? op))
                           [(butlast args) (last args)]
                           (let [args (drop-last 2 args)]
                             (case op
                               ;; return a function that calls the computation fn
                               ;;  on the input signal, removing the query vector
                               :->
                               [args (fn [db _]
                                       (f db))]
                               ;; return a function that calls the computation fn
                               ;;  on the input signal and the data in the query vector
                               ;;  that is not the query-id
                               :=>
                               [args (fn [db q]
                                       (if (map? q)
                                         (f db q)
                                         (let [[_ & qs] q]
                                           (apply f db qs))))]
                               ;; an incorrect keyword was passed
                               (console :error error-header "expected :-> or :=> as second to last argument, got:" op))))
        inputs-fn (case (count input-args)
                    ;; no `inputs` function provided - give the default
                    0 (fn
                        ([_] app-db)
                        ([_ _] app-db))

                    ;; a single `inputs` fn
                    1 (let [f (first input-args)]
                        (when-not (fn? f)
                          (console :error error-header "2nd argument expected to be an inputs function, got:" f))
                        f)

                    ;; one sugar pair
                    2 (let [[marker vec] input-args]
                        (when-not (= :<- marker)
                          (console :error error-header "expected :<-, got:" marker))
                        (fn inp-fn
                          ([_] (sub-fn vec))
                          ([_ _] (sub-fn vec))))

                    ;; multiple sugar pairs
                    (let [pairs   (partition 2 input-args)
                          markers (map first pairs)
                          vecs    (map second pairs)]
                      (when-not (and (every? #{:<-} markers) (every? query? vecs))
                        (console :error error-header "expected pairs of :<- and vectors, got:" pairs))
                      (fn inp-fn
                        ([_] (map sub-fn vecs))
                        ([_ _] (map sub-fn vecs)))))]
    [inputs-fn computation-fn]))

(defn reg-sub
  [query-id & args]
  (let [[inputs-fn computation-fn] (apply sugar query-id subscribe vector? args)]
    (register-handler
     kind
     query-id
     (fn subs-handler-fn
       ([db query-vec]
        (let [subscriptions (inputs-fn query-vec nil)
              reaction-id   (atom nil)
              reaction      (make-reaction
                             (fn []
                               (trace/with-trace {:operation (first-in-vector query-vec)
                                                  :op-type   :sub/run
                                                  :tags      {:query-v    query-vec
                                                              :reaction   @reaction-id}}
                                 (let [subscription (computation-fn (deref-input-signals subscriptions query-id) query-vec)]
                                   (trace/merge-trace! {:tags {:value subscription}})
                                   subscription))))]

          (reset! reaction-id (reagent-id reaction))
          reaction))
       ([db query-vec dyn-vec]
        (let [subscriptions (inputs-fn query-vec dyn-vec)
              reaction-id   (atom nil)
              reaction      (make-reaction
                             (fn []
                               (trace/with-trace {:operation (first-in-vector query-vec)
                                                  :op-type   :sub/run
                                                  :tags      {:query-v   query-vec
                                                              :dyn-v     dyn-vec
                                                              :reaction  @reaction-id}}
                                 (let [subscription (computation-fn (deref-input-signals subscriptions query-id) query-vec dyn-vec)]
                                   (trace/merge-trace! {:tags {:value subscription}})
                                   subscription))))]

          (reset! reaction-id (reagent-id reaction))
          reaction))))))
````

## File: README.md
````markdown
<p align="center"><a href="https://day8.github.io/re-frame" target="_blank" rel="noopener noreferrer"><img src="docs/images/logo/re-frame-colour.png?raw=true" alt="re-frame logo"></a></p>

## Derived Values, Flowing

> This, milord, is my family's axe. We have owned it for almost nine hundred years, see. Of course,
sometimes it needed a new blade. And sometimes it has required a new handle, new designs on the
metalwork, a little refreshing of the ornamentation ... but is this not the nine hundred-year-old
axe of my family? And because it has changed gently over time, it is still a pretty good axe,
y'know. Pretty good.

> -- Terry Pratchett, The Fifth Elephant <br>
> &nbsp;&nbsp;&nbsp; reflecting on identity, flow and derived values  (aka [The Ship of Theseus](https://en.wikipedia.org/wiki/Ship_of_Theseus))
<br/>
<br/>

<!--
[![CI](https://github.com/day8/re-frame/workflows/ci/badge.svg)](https://github.com/day8/re-frame/actions?workflow=ci)
[![CD](https://github.com/day8/re-frame/workflows/cd/badge.svg)](https://github.com/day8/re-frame/actions?workflow=cd)
[![License](https://img.shields.io/github/license/day8/re-frame.svg)](license.txt)
-->

## Overview

re-frame is a ClojureScript framework for building user interfaces.
It has a data-oriented, functional design. Its primary focus is on high programmer productivity and scaling up to larger Single-Page applications.

Developed in late 2014, and released in 2015, it is mature and stable. It is used by both small startups and companies with over 500 developers, and it has delivered into production applications which are 40K lines of code and beyond.

Across the last 6 years, it has outlasted multiple generations of Javascript churn - just imagine your team's productivity if you didn't have to contend with technical churn, and have new magic burn your fingers every two years. Brand new, exciting concepts like recoiljs (in the React world), have been a regular part of re-frame from the beginning.

re-frame is lucky enough to enjoy an unfair advantage - ClojureScript is a Lisp. Alan Kay
once described Lisp as "Maxwell's equations of software". Paul Graham
described how Lisp was a competitive advantage for his startup.  When we use Lisp, we
get to leverage 50 years of foliated excellence from the very best minds available.
And then there's also a thriving ClojureScript community which delivers modern ideas and best-in-class tooling.

Although re-frame leverages React (via Reagent), it only needs
React to be the V in MVC, and no more. re-frame takes a different road to the currently-pervasive idea that Views should be causal (colocated queries, ComponentDidMount, hooks, etc).
In re-frame, events are causal, and views are purely reactive.

## Documentation

The re-frame documentation is [available here](https://day8.github.io/re-frame/).


## The Current Version

[![Clojars Project](https://img.shields.io/clojars/v/re-frame?labelColor=283C67&color=729AD1&style=for-the-badge&logo=clojure&logoColor=fff)](https://clojars.org/re-frame)

For full dependency information, see the [Clojars page](https://clojars.org/re-frame/)

## Getting Help

[![Get help on Slack](http://img.shields.io/badge/slack-clojurians%20%23re--frame-97C93C?labelColor=283C67&logo=slack&style=for-the-badge)](https://clojurians.slack.com/channels/re-frame)

## Licence

re-frame is [MIT licenced](license.txt)
````
