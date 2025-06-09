# Reitit expert

Project Path: doc

Source Tree:

```
doc
├── basics
│   ├── router.md
│   ├── route_syntax.md
│   ├── path_based_routing.md
│   ├── route_data.md
│   ├── route_conflicts.md
│   ├── error_messages.md
│   ├── route_data_validation.md
│   └── name_based_routing.md
├── advanced
│   ├── dev_workflow.md
│   ├── different_routers.md
│   ├── shared_routes.md
│   ├── route_validation.md
│   ├── configuring_routers.md
│   └── composing_routers.md
├── frontend
│   ├── basics.md
│   ├── coercion.md
│   ├── browser.md
│   └── controllers.md
├── SUMMARY.md
├── images
│   ├── ring-request-diff.png
│   ├── pretty-error.png
│   ├── invalid_roles.png
│   ├── tfb_json.png
│   ├── http-context-diff.png
│   ├── closed-spec2.png
│   ├── closed-spec1.png
│   ├── reitit.png
│   ├── opensensors.png
│   ├── lupapiste.png
│   ├── conflicts1.png
│   ├── swagger.png
│   └── conflicts2.png
├── cljdoc.edn
├── faq.md
├── performance.md
├── README.md
├── http
│   ├── transforming_interceptor_chain.md
│   ├── pedestal.md
│   ├── interceptors.md
│   ├── sieppari.md
│   └── default_interceptors.md
├── ring
│   ├── middleware_registry.md
│   ├── default_handler.md
│   ├── exceptions.md
│   ├── static.md
│   ├── coercion.md
│   ├── default_middleware.md
│   ├── ring.md
│   ├── RESTful_form_methods.md
│   ├── dynamic_extensions.md
│   ├── content_negotiation.md
│   ├── swagger.md
│   ├── route_data_validation.md
│   ├── slash_handler.md
│   ├── data_driven_middleware.md
│   ├── transforming_middleware_chain.md
│   ├── reverse_routing.md
│   ├── openapi.md
│   └── compiling_middleware.md
├── development.md
└── coercion
    ├── schema_coercion.md
    ├── coercion.md
    ├── clojure_spec_coercion.md
    ├── malli_coercion.md
    └── data_spec_coercion.md

```

`/Users/ovistoica/workspace/reitit/doc/basics/router.md`:

```md
# Router

Routes are just data and to do routing, we need a router instance satisfying the `reitit.core/Router` protocol. Routers are created with `reitit.core/router` function, taking the raw routes and optionally an options map.

The `Router` protocol:

```clj
(defprotocol Router
  (router-name [this])
  (routes [this])
  (options [this])
  (route-names [this])
  (match-by-path [this path])
  (match-by-name [this name] [this name params]))
```

Creating a router:

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    ["/api"
     ["/ping" ::ping]
     ["/user/:id" ::user]]))
```

Name of the created router:

```clj
(r/router-name router)
; :mixed-router
```

The flattened route tree:

```clj
(r/routes router)
; [["/api/ping" {:name :user/ping}]
;  ["/api/user/:id" {:name :user/user}]]
```

With a router instance, we can do [Path-based routing](path_based_routing.md) or [Name-based (Reverse) routing](name_based_routing.md).

## More details

Router options:

```clj
(r/options router)
{:lookup #object[...]
 :expand #object[...]
 :coerce #object[...]
 :compile #object[...]
 :conflicts #object[...]}
```

Route names:

```clj
(r/route-names router)
; [:user/ping :user/user]
```

### Composing

As routes are defined as plain data, it's easy to merge multiple route trees into a single router

```clj
(def user-routes
  [["/users" ::users]
   ["/users/:id" ::user]])

(def admin-routes
  ["/admin"
   ["/ping" ::ping]
   ["/db" ::db]])

(def router
  (r/router
    [admin-routes
     user-routes]))
```

Merged route tree:

```clj
(r/routes router)
; [["/admin/ping" {:name :user/ping}]
;  ["/admin/db" {:name :user/db}]
;  ["/users" {:name :user/users}]
;  ["/users/:id" {:name :user/user}]]
```

More details on [composing routers](../advanced/composing_routers.md).

### Behind the scenes

When router is created, the following steps are done:
* route tree is flattened
* route arguments are expanded (via `:expand` option)
* routes are coerced (via `:coerce` options)
* route tree is compiled (via `:compile` options)
* [route conflicts](route_conflicts.md) are resolved (via `:conflicts` options)
* optionally, route data is validated (via `:validate` options)
* [router implementation](../advanced/different_routers.md) is automatically selected (or forced via `:router` options) and created

```

`/Users/ovistoica/workspace/reitit/doc/basics/route_syntax.md`:

```md
# Route Syntax

Routes are defined as vectors of:
- path (a string)
- optional route data: usually a map, but see [Route Data](./route_data.md)
- any number of child routes

Routes can be wrapped in vectors and lists and `nil` routes are ignored.

Paths can have path-parameters (`:id`) or catch-all-parameters (`*path`). Parameters can also be wrapped in brackets, enabling use of qualified keywords `{user/id}`, `{*user/path}`. By default, both syntaxes are supported, see [configuring routers](../advanced/configuring_routers.md) on how to change this.

### Examples

Simple route:

```clj
["/ping" {:handler ping}]
```

Two routes with more data:

```clj
[["/ping" {:handler ping
           :cost 300}]
 ["/pong" {:handler pong
           :tags #{:game}}]]
```

Routes with path parameters (see also [Coercion](../coercion/coercion.md) and [Ring Coercion](../ring/coercion.md)):

```clj
[["/users/:user-id" {:handler get-user}]
 ["/api/:version/ping" {:handler ping-version}]]
```

```clj
[["/users/{user-id}" {:handler get-user}]
 ["/files/file-{number}.pdf" {:handler get-pdf}]]
```

Route with catch-all parameter:

```clj
["/public/*path" {:handler get-file}]
```

```clj
["/public/{*path}" {:handler get-file}]
```

Nested routes:

```clj
["/api"
 ["/admin" {:middleware [::admin]}
  ["" {:name ::admin}]
  ["/db" {:name ::db}]]
 ["/ping" {:name ::ping}]]
```

Same routes flattened:

```clj
[["/api/admin" {:middleware [::admin], :name ::admin}]
 ["/api/admin/db" {:middleware [::admin], :name ::db}]
 ["/api/ping" {:name ::ping}]]
```

### Encoding

Reitit does not apply any encoding to your paths. If you need that, you must encode them yourself. E.g., `/foo bar` should be `/foo%20bar`.

### Wildcards

Normal path-parameters (`:id`) can start anywhere in the path string, but have to end either to slash `/` (currently hardcoded) or to an end of path string:

```clj
[["/api/:version" {...}]
 ["/files/file-:number" {...}]
 ["/user/:user-id/orders" {...}]]
```

Bracket path-parameters can start and stop anywhere in the path-string, the following character is used as a terminator.

```clj
[["/api/{version}" {...}]
 ["/files/{name}.{extension}" {...}]
 ["/user/{user-id}/orders" {...}]]
```

Having multiple terminators after a bracket path-path parameter with identical path prefix will cause a compile-time error at router creation:

```clj
[["/files/file-{name}.pdf" {...}]            ;; terminator \.
 ["/files/file-{name}-{version}.pdf" {...}]] ;; terminator \-
```

### Slash Free Routing

```clj
[["broker.{customer}.{device}.{*data}" {...}]
 ["events.{target}.{type}" {...}]]
```

### Generating routes

Routes are just data, so it's easy to create them programmatically:

```clj
(defn cqrs-routes [actions]
  ["/api" {:interceptors [::api ::db]}
   (for [[type interceptor] actions
         :let [path (str "/" (name interceptor))
               method (case type
                        :query :get
                        :command :post)]]
     [path {method {:interceptors [interceptor]}}])])
```

```clj
(cqrs-routes
  [[:query   'get-user]
   [:command 'add-user]
   [:command 'add-order]])
; ["/api" {:interceptors [::api ::db]}
;  (["/get-user" {:get {:interceptors [get-user]}}]
;   ["/add-user" {:post {:interceptors [add-user]}}]
;   ["/add-order" {:post {:interceptors [add-order]}}])]
```

### Explicit path-parameter syntax

Router options `:syntax` allows the path-parameter syntax to be explicitly defined. It takes a keyword or set of keywords as a value. Valid values are `:colon` and `:bracket`. Default value is `#{:colon :bracket}`.

With defaults:

```clj
(-> (r/router
      ["http://localhost:8080/api/user/{id}" ::user-by-id])
    (r/match-by-path "http://localhost:8080/api/user/123"))
;#Match{:template "http://localhost:8080/api/user/{id}",
;       :data {:name :user/user-by-id},
;       :result nil,
;       :path-params {:id "123", :8080 ":8080"},
;       :path "http://localhost:8080/api/user/123"}
```

Supporting only `:bracket` syntax:

```clj
(require '[reitit.core :as r])

(-> (r/router
      ["http://localhost:8080/api/user/{id}" ::user-by-id]
      {:syntax :bracket})
    (r/match-by-path "http://localhost:8080/api/user/123"))
;#Match{:template "http://localhost:8080/api/user/{id}",
;       :data {:name :user/user-by-id},
;       :result nil,
;       :path-params {:id "123"},
;       :path "http://localhost:8080/api/user/123"}
```

```

`/Users/ovistoica/workspace/reitit/doc/basics/path_based_routing.md`:

```md
# Path-based Routing

Path-based routing is done using the `reitit.core/match-by-path` function. It takes the router and path as arguments and returns one of the following:

* `nil`, no match
* `PartialMatch`, path matched, missing path-parameters (only in reverse-routing)
* `Match`, an exact match

Given a router:

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    ["/api"
     ["/ping" ::ping]
     ["/user/:id" ::user]]))
```

No match returns `nil`:

```clj
(r/match-by-path router "/hello")
; nil
```

Match provides the route information:

```clj
(r/match-by-path router "/api/user/1")
; #Match{:template "/api/user/:id"
;        :data {:name :user/user}
;        :path "/api/user/1"
;        :result nil
;        :path-params {:id "1"}}
```

```

`/Users/ovistoica/workspace/reitit/doc/basics/route_data.md`:

```md
# Route Data

Route data is the key feature of reitit. Routes can have any map-like data attached to them, to be interpreted by the client application, `Router` or routing components like `Middleware` or `Interceptors`.

```clj
[["/ping" {:name ::ping}]
 ["/pong" {:handler identity}]
 ["/users" {:get {:roles #{:admin}
                  :handler identity}}]]
```

Besides map-like data, raw routes can have any non-sequential route argument after the path. This argument is expanded by `Router` (via `:expand` option) into route data at router creation time.

By default, Keywords are expanded into `:name` (see [Name-based Routing](./name_based_routing.md))
and functions into `:handler` keys.

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    [["/ping" ::ping]
     ["/pong" identity]
     ["/users" {:get {:roles #{:admin}
                      :handler identity}}]]))
```

## Using Route Data

Expanded route data can be retrieved from a router with `routes` and is returned with `match-by-path` and `match-by-name` in case of a route match.

```clj
(r/routes router)
; [["/ping" {:name ::ping}]
;  ["/pong" {:handler identity]}
;  ["/users" {:get {:roles #{:admin}
;                   :handler identity}}]]
```

```clj
(r/match-by-path router "/ping")
; #Match{:template "/ping"
;        :data {:name :user/ping}
;        :result nil
;        :path-params {}
;        :path "/ping"}
```

```clj
(r/match-by-name router ::ping)
; #Match{:template "/ping"
;        :data {:name :user/ping}
;        :result nil
;        :path-params {}
;        :path "/ping"}
```

## Nested Route Data

For nested route trees, route data is accumulated recursively from root towards leafs using [meta-merge](https://github.com/weavejester/meta-merge). Default behavior for collections is `:append`, but this can be overridden to `:prepend`, `:replace` or `:displace` using the target meta-data.

An example router with nested data:

```clj
(def router
  (r/router
    ["/api" {:interceptors [::api]}
     ["/ping" ::ping]
     ["/admin" {:roles #{:admin}}
      ["/users" ::users]
      ["/db" {:interceptors [::db]
              :roles ^:replace #{:db-admin}}]]]))
```

Resolved route tree:

```clj
(r/routes router)
; [["/api/ping" {:interceptors [::api]
;                :name :user/ping}]
;  ["/api/admin/users" {:interceptors [::api]
;                       :roles #{:admin}
;                       :name ::users}]
;  ["/api/admin/db" {:interceptors [::api ::db]
;                    :roles #{:db-admin}}]]
```

See also [nested parameter definitions for coercions](../ring/coercion.md#nested-parameter-definitions)

## Route Data Fragments

Just like [fragments in React.js](https://reactjs.org/docs/fragments.html), we can create routing tree fragments by using empty path `""`. This allows us to add route data without accumulating to path.

Given a route tree:

```clj
[["/swagger.json" ::swagger]
 ["/api-docs" ::api-docs]
 ["/api/ping" ::ping]
 ["/api/pong" ::pong]]
```

Adding `:no-doc` route data to exclude the first routes from generated [Swagger documentation](../ring/swagger.md):

```clj
[["" {:no-doc true}
  ["/swagger.json" ::swagger]
  ["/api-docs" ::api-docs]]
 ["/api/ping" ::ping]
 ["/api/pong" ::pong]]
```

Accumulated route data:

```clj
(def router
  (r/router
    [["" {:no-doc true}
      ["/swagger.json" ::swagger]
      ["/api-docs" ::api-docs]]
     ["/api/ping" ::ping]
     ["/api/pong" ::pong]]))

(r/routes router)
; [["/swagger.json" {:no-doc true, :name ::swagger}]
;  ["/api-docs" {:no-doc true, :name ::api-docs}]
;  ["/api/ping" {:name ::ping}]
;  ["/api/pong" {:name ::pong}]]
```

## Top-level Route Data

Route data can be introduced also via `Router` option `:data`:

```clj
(def router
  (r/router
    ["/api"
     {:middleware [::api]}
     ["/ping" ::ping]
     ["/pong" ::pong]]
    {:data {:middleware [::session]}}))
```

Expanded routes:

```clj
[["/api/ping" {:middleware [::session ::api], :name ::ping}]
 ["/api/pong" {:middleware [::session ::api], :name ::pong}]]
```


## Customizing Expansion

By default, router `:expand` option has value `r/expand` function, backed by a `r/Expand` protocol. Expansion can be customized either by swapping the `:expand` implementation or by extending the Protocol. `r/Expand` implementations can be recursive.

Naive example to add direct support for `java.io.File` route argument:

```clj
(extend-type java.io.File
  r/Expand
  (expand [file options]
    (r/expand
      #(slurp file)
      options)))

(r/router
  ["/" (java.io.File. "index.html")])
```

Page [shared routes](../advanced/shared_routes.md#using-custom-expander) has an example of an custom `:expand` implementation.

## Route data validation

See [Route data validation](route_data_validation.md).

```

`/Users/ovistoica/workspace/reitit/doc/basics/route_conflicts.md`:

```md
# Route Conflicts

We should fail fast if a router contains conflicting paths or route names.

When a `Router` is created via `reitit.core/router`, both path and route name conflicts are checked automatically. By default, in case of conflict, an `ex-info` is thrown with a descriptive message. In some (legacy api) cases, path conflicts should be allowed and one can override the path conflict resolution via `:conflicts` router option or via `:conflicting` route data.

## Path Conflicts

Routes with path conflicts:

```clj
(require '[reitit.core :as r])

(def routes
  [["/ping"]
   ["/:user-id/orders"]
   ["/bulk/:bulk-id"]
   ["/public/*path"]
   ["/:version/status"]])
```

Creating router with defaults:

```clj
(r/router routes)
; CompilerException clojure.lang.ExceptionInfo: Router contains conflicting route paths:
;
; -> /:user-id/orders
; -> /public/*path
; -> /bulk/:bulk-id
;
; -> /bulk/:bulk-id
; -> /:version/status
;
; -> /public/*path
; -> /:version/status
;
```

To ignore the conflicts:

```clj
(r/router
  routes
  {:conflicts nil})
; => #object[reitit.core$quarantine_router$reify
```

To just log the conflicts:

```clj
(require '[reitit.exception :as exception])

(r/router
  routes
  {:conflicts (fn [conflicts]
                (println (exception/format-exception :path-conflicts nil conflicts)))})
; Router contains conflicting route paths:
;
; -> /:user-id/orders
; -> /public/*path
; -> /bulk/:bulk-id
;
; -> /bulk/:bulk-id
; -> /:version/status
;
; -> /public/*path
; -> /:version/status
;
; => #object[reitit.core$quarantine_router$reify]
```

Alternatively, you can ignore conflicting paths individually via `:conflicting` in route data:

```clj
(def routes
  [["/ping"]
   ["/:user-id/orders" {:conflicting true}]
   ["/bulk/:bulk-id" {:conflicting true}]
   ["/public/*path" {:conflicting true}]
   ["/:version/status" {:conflicting true}]])
; => #'user/routes
(r/router routes)
;  => #object[reitit.core$quarantine_router$reify]
```

## Name conflicts

Routes with name conflicts:

```clj
(def routes
  [["/ping" ::ping]
   ["/admin" ::admin]
   ["/admin/ping" ::ping]])
```

Creating router with defaults:

```clj
(r/router routes)
;CompilerException clojure.lang.ExceptionInfo: Router contains conflicting route names:
;
;:reitit.core/ping
;-> /ping
;-> /admin/ping
;
```

There is no way to disable the name conflict resolution.

```

`/Users/ovistoica/workspace/reitit/doc/basics/error_messages.md`:

```md
# Error Messages

All exceptions thrown in router creation are caught, formatted and rethrown by the `reitit.core/router` function. Exception formatting is done by the exception formatter defined by the `:exception` router option.

## Default Errors

The default exception formatting uses `reitit.exception/exception`. It produces single-color, partly human-readable, error messages.

```clj
(require '[reitit.core :as r])

(r/router
  [["/ping"]
   ["/:user-id/orders"]
   ["/bulk/:bulk-id"]
   ["/public/*path"]
   ["/:version/status"]])
```

![Pretty error](../images/conflicts1.png)

## Pretty Errors

```clj
[metosin/reitit-dev "0.8.0"]
```

For human-readable and developer-friendly exception messages, there is `reitit.dev.pretty/exception` (in the `reitit-dev` module). It is inspired by the lovely errors messages of [ELM](https://elm-lang.org/blog/compiler-errors-for-humans) and [ETA](https://twitter.com/jyothsnasrin/status/1037703436043603968) and uses [fipp](https://github.com/brandonbloom/fipp), [expound](https://github.com/bhb/expound) and [spell-spec](https://github.com/bhauman/spell-spec) for most of heavy lifting.

```clj
(require '[reitit.dev.pretty :as pretty])

(r/router
  [["/ping"]
   ["/:user-id/orders"]
   ["/bulk/:bulk-id"]
   ["/public/*path"]
   ["/:version/status"]]
  {:exception pretty/exception})
```

![Pretty error](../images/conflicts2.png)

## Extending

Behind the scenes, both error formatters are backed by a multimethod, so they are easy to extend.

## More examples

See the [validating route data](route_data_validation.md) page.

## Runtime Exception

See [Exception Handling with Ring](../ring/exceptions.md).

```

`/Users/ovistoica/workspace/reitit/doc/basics/route_data_validation.md`:

```md
# Route Data Validation

Route data can be anything, so it's easy to go wrong. Accidentally adding a `:role` key instead of `:roles` might hinder the whole routing app without any authorization in place.

To fail fast, we could use the custom `:coerce` and `:compile` hooks to apply data validation and throw exceptions on first sighted problem.

But there is a better way. Router has a `:validation` hook to validate the whole route tree after it's successfully compiled. It expects a 2-arity function `routes opts => ()` that can side-effect in case of validation errors.

## clojure.spec

Namespace `reitit.spec` contains specs for main parts of `reitit.core` and a helper function `validate` that runs spec validation for all route data and throws an exception if any errors are found.

A Router with invalid route data:

```clj
(require '[reitit.core :as r])

(r/router
  ["/api" {:handler "identity"}])
; #object[reitit.core$...]
```

Failing fast with `clojure.spec` validation turned on:

```clj
(require '[reitit.spec :as rs])

(r/router
  ["/api" {:handler "identity"}]
  {:validate rs/validate})
; CompilerException clojure.lang.ExceptionInfo: Invalid route data:
;
; -- On route -----------------------
;
; "/api"
;
; In: [:handler] val: "identity" fails spec: :reitit.spec/handler at: [:handler] predicate: fn?
;
; {:problems (#reitit.spec.Problem{:path "/api", :scope nil, :data {:handler "identity"}, :spec :reitit.spec/default-data, :problems #:clojure.spec.alpha{:problems ({:path [:handler], :pred clojure.core/fn?, :val "identity", :via [:reitit.spec/default-data :reitit.spec/handler], :in [:handler]}), :spec :reitit.spec/default-data, :value {:handler "identity"}}})}, compiling: ...

```

### Pretty errors

Turning on [Pretty Errors](error_messages.md#pretty-errors) will give much nicer error messages:

```clj
(require '[reitit.dev.pretty :as pretty])

(r/router
  ["/api" {:handler "identity"}]
  {:validate rs/validate
   :exception pretty/exception})
```

![Pretty error](../images/pretty-error.png)

### Customizing spec validation

`rs/validate` reads the following router options:

  | key                 | description |
  | --------------------|-------------|
  | `:spec`             | the spec to verify the route data (default `::rs/default-data`)
  | `:reitit.spec/wrap` | function of `spec => spec` to wrap all route specs

**NOTE**: `clojure.spec` implicitly validates all values with fully-qualified keys if specs exist with the same name.

Invalid spec value:

```clj
(require '[clojure.spec.alpha :as s])

(s/def ::role #{:admin :manager})
(s/def ::roles (s/coll-of ::role :into #{}))

(r/router
  ["/api" {:handler identity
           ::roles #{:adminz}}]
  {:validate rs/validate
  :exception pretty/exception})
```

![Invalid Role Error](../images/invalid_roles.png)

## Closed Specs

To fail-fast on non-defined and misspelled keys on route data, we can close the specs using `:reitit.spec/wrap` options with value of `spec-tools.spell/closed` that closed the top-level specs.

Requiring a`:description` and validating using closed specs:

```clj
(require '[spec-tools.spell :as spell])

(s/def ::description string?)

(r/router
  ["/api" {:summary "kikka"}]
  {:validate rs/validate
   :spec (s/merge ::rs/default-data
                  (s/keys :req-un [::description]))
   ::rs/wrap spell/closed
   :exception pretty/exception})
```

![Closed Spec error](../images/closed-spec1.png)

It catches also typing errors:

```clj
(r/router
  ["/api" {:descriptionz "kikka"}]
  {:validate rs/validate
   :spec (s/merge ::rs/default-data
                  (s/keys :req-un [::description]))
   ::rs/wrap spell/closed
   :exception pretty/exception})
```

![Closed Spec error](../images/closed-spec2.png)


```

`/Users/ovistoica/workspace/reitit/doc/basics/name_based_routing.md`:

```md
# Name-based (reverse) Routing

All routes which have `:name` route data defined can also be matched by name.

Given a router:

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    ["/api"
     ["/ping" ::ping]
     ["/user/:id" ::user]]))
```

Listing all route names:

```clj
(r/route-names router)
; [:user/ping :user/user]
```

No match returns `nil`:

```clj
(r/match-by-name router ::kikka)
nil
```

Matching a route:

```clj
(r/match-by-name router ::ping)
; #Match{:template "/api/ping"
;        :data {:name :user/ping}
;        :result nil
;        :path-params {}
;        :path "/api/ping"}
```

If not all path-parameters are set, a `PartialMatch` is returned:

```clj
(r/match-by-name router ::user)
; #PartialMatch{:template "/api/user/:id",
;               :data {:name :user/user},
;               :result nil,
;               :path-params nil,
;               :required #{:id}}

(r/partial-match? (r/match-by-name router ::user))
; true
```

With provided path-parameters:

```clj
(r/match-by-name router ::user {:id "1"})
; #Match{:template "/api/user/:id"
;        :data {:name :user/user}
;        :path "/api/user/1"
;        :result nil
;        :path-params {:id "1"}}
```

Path-parameters are automatically coerced into strings, with the help of (currently internal) Protocol `reitit.impl/IntoString`. It supports strings, numbers, booleans, keywords and objects:

```clj
(r/match-by-name router ::user {:id 1})
; #Match{:template "/api/user/:id"
;        :data {:name :user/user}
;        :path "/api/user/1"
;        :result nil
;        :path-params {:id "1"}}
```

There is also an exception throwing version:

```clj
(r/match-by-name! router ::user)
; ExceptionInfo missing path-params for route /api/user/:id: #{:id}
```

To turn a Match into a path, there is `reitit.core/match->path`:

```clj
(-> router
    (r/match-by-name ::user {:id 1})
    (r/match->path))
; "/api/user/1"
```

It can take an optional map of query-parameters too:

```clj
(-> router
    (r/match-by-name ::user {:id 1})
    (r/match->path {:iso "möly"}))
; "/api/user/1?iso=m%C3%B6ly"
```

```

`/Users/ovistoica/workspace/reitit/doc/advanced/dev_workflow.md`:

```md
# Dev Workflow

Many applications will require the routes to span multiple namespaces. It is quite easy to do so with reitit, but we might hit a problem during development.

## An example

Consider this sample routing :

```clj
(ns ns1)

(def routes
  ["/bar" ::bar])

(ns ns2)
(require '[ns1])

(def routes
  [["/ping" ::ping]
   ["/more" ns1/routes]])

(ns ns3)
(require '[ns1])
(require '[ns2])
(require '[reitit.core :as r])

(def routes
  ["/api"
   ["/ns2" ns2/routes]
   ["/ping" ::ping]])

(def router (r/router routes))
```

We may query the top router and get the expected result :
```clj
(r/match-by-path router "/api/ns2/more/bar")
;#reitit.core.Match{:template "/api/ns2/more/bar", :data {:name :ns1/bar}, :result nil, :path-params {}, :path "/api/ns2/more/bar"}
```

Notice the route name : ```:ns1/bar```

When we change the routes in ```ns1``` like this :
```clj
(ns ns1
  (:require [reitit.core :as r]))

(def routes
  ["/bar" ::bar-with-new-name])
```

After we recompile the ```ns1``` namespace, and query again
```clj
ns1/routes
;["/bar" :ns1/bar-with-new-name]
;The routes var in ns1 was changed indeed

(r/match-by-path router "/api/ns2/more/bar")
;#reitit.core.Match{:template "/api/ns2/more/bar", :data {:name :ns1/bar}, :result nil, :path-params {}, :path "/api/ns2/more/bar"}
```

The route name is still ```:ns1/bar``` !

While we could use the [reloaded workflow](http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded) to reload the whole routing tree, it is not always possible, and quite frankly a bit slower than we might want for fast iterations.

## A crude solution

In order to see the changes without reloading the whole route tree, we can use functions.

```clj
(ns ns1)

(defn routes [] ;; Now a function !
  ["/bar" ::bar])

(ns ns2)
(require '[ns1])

(defn routes [] ;; Now a function !
  [["/ping" ::ping]
   ["/more" (ns1/routes)]]) ;; Now a function call

(ns ns3)
(require '[ns1])
(require '[ns2])
(require '[reitit.core :as r])

(defn routes [] ;; Now a function !
  ["/api"
   ["/ns2" (ns2/routes)] ;; Now a function call
   ["/ping" ::ping]])

(def router #(r/router (routes))) ;; Now a function
```

Let's query again

```clj
(r/match-by-path (router) "/api/ns2/more/bar")
;#reitit.core.Match{:template "/api/ns2/more/bar", :data {:name :ns1/bar}, :result nil, :path-params {}, :path "/api/ns2/more/bar"}
```

Notice that's we're now calling a function rather than just passing ```router``` to the matching function.

Now let's again change the route name in ```ns1```, and recompile that namespace.

```clj
(ns ns1)

(defn routes []
  ["/bar" ::bar-with-new-name])
```

let's see the query result :

```clj
(r/match-by-path (router) "/api/ns2/more/bar")
;#reitit.core.Match{:template "/api/ns2/more/bar", :data {:name :ns1/bar-with-new-name}, :result nil, :path-params {}, :path "/api/ns2/more/bar"}
```

Notice that the name is now correct, without reloading every namespace under the sun.

## Why is this a crude solution ?

The astute reader will have noticed that we're recompiling the full routing tree on every invocation. While this solution is practical during development, it goes contrary to the performance goals of reitit.

We need a way to only do this once at production time.

## An easy fix

Let's apply a small change to our ```ns3```. We'll replace our router by two different routers, one for dev and one for production.

```clj
(ns ns3)
(require '[ns1])
(require '[ns2])
(require '[reitit.core :as r])

(defn routes []
  ["/api"
   ["/ns2" (ns2/routes)]
   ["/ping" ::ping]])

(def dev-router #(r/router (routes))) ;; A router for dev
(def prod-router (constantly (r/router (routes)))) ;; A router for prod
```

And there you have it, dynamic during dev, performance at production. We have it all !

## Var handlers

You can use a var instead of a function as a `:handler`. This will
allow you to modify the handler function without rebuilding the reitit
router.

For example:

```clj
(def router
  (ring/router
    ["/ping" {:get #'my-ns/handler}]))
```

Now you can reload `my-ns` or redefine `my-ns/handler` and the router
will use the new definition automatically.

```

`/Users/ovistoica/workspace/reitit/doc/advanced/different_routers.md`:

```md
# Different Routers

Reitit ships with several different implementations for the `Router` protocol, originally based on the [Pedestal](https://github.com/pedestal/pedestal/tree/master/route) implementation. `router` function selects the most suitable implementation by inspecting the expanded routes. The implementation can be set manually using `:router` option, see [configuring routers](configuring_routers.md).

| router                        | description |
| ------------------------------|-------------|
| `:linear-router`              | Matches the routes one-by-one starting from the top until a match is found. Slow, but works with all route trees.
| `:trie-router`                | Router that creates a optimized [search trie](https://en.wikipedia.org/wiki/Trie) out of an route table. Much faster than `:linear-router` for wildcard routes. Valid only if there are no [Route conflicts](../basics/route_conflicts.md).
| `:lookup-router`              | Fast router, uses hash-lookup to resolve the route. Valid if no paths have path or catch-all parameters and there are no [Route conflicts](../basics/route_conflicts.md).
| `:single-static-path-router`  | Super fast router: string-matches a route. Valid only if there is one static route.
| `:mixed-router`               | Contains two routers: `:trie-router` for wildcard routes and a `:lookup-router` or `:single-static-path-router` for static routes. Valid only if there are no [Route conflicts](../basics/route_conflicts.md).
| `:quarantine-router`          | Contains two routers: `:mixed-router` for non-conflicting routes and a `:linear-router` for conflicting routes.

The router name can be asked from the router:

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    [["/ping" ::ping]
     ["/api/:users" ::users]]))

(r/router-name router)
; :mixed-router
```

Overriding the router implementation:

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    [["/ping" ::ping]
     ["/api/:users" ::users]]
    {:router r/linear-router}))

(r/router-name router)
; :linear-router
```

```

`/Users/ovistoica/workspace/reitit/doc/advanced/shared_routes.md`:

```md
# Shared routes

As `reitit-core` works with both Clojure & ClojureScript, one can have a shared routing table for both the frontend and the backend application, using the [Clojure Common Files](https://clojure.org/guides/reader_conditionals).

For backend, you need to define a `:handler` for the request processing, for frontend, `:name` enables the use of [reverse routing](../basics/name_based_routing.md).

There are multiple options to use shared routing table.

## Using reader conditionals

```clj
;; define the handlers for clojure
#?(:clj (declare get-kikka))
#?(:clj (declare post-kikka))

;; :name for both, :handler just for clojure
(def routes
  ["/kikka"
   {:name ::kikka
    #?@(:clj [:get {:handler get-kikka}])
    #?@(:clj [:post {:handler post-kikka}])}])
```

## Using custom expander

raw-routes can have any non-sequential data as a route argument, which gets expanded using the `:expand` option given to the `reitit.core.router` function. It defaults to `reitit.core/expand` multimethod.

First, define the common routes (in a `.cljc` file):

```clj
(def routes
  [["/kikka" ::kikka]
   ["/bar" ::bar]])
```

Those can be used as-is from ClojureScript:

```clj
(require '[reitit.core :as r])

(def router
  (r/router routes))

(r/match-by-name router ::kikka)
;#Match{:template "/kikka"
;       :data {:name :user/kikka}
;       :result nil
;       :path-params nil
;       :path "/kikka"}
```

For the backend, we can use a custom-expander to expand the routes:

```clj
(require '[reitit.ring :as ring])
(require '[reitit.core :as r])

(defn my-expand [registry]
  (fn [data opts]
    (if (keyword? data)
      (some-> data
              registry
              (r/expand opts)
              (assoc :name data))
      (r/expand data opts))))

;; the handler functions
(defn get-kikka [_] {:status 200, :body "get"})
(defn post-kikka [_] {:status 200, :body "post"})
(defn bar [_] {:status 200, :body "bar"})

(def app
  (ring/ring-handler
    (ring/router
      [["/kikka" ::kikka]
       ["/bar" ::bar]]
      ;; use a custom expander
      {:expand (my-expand
                 {::kikka {:get get-kikka
                           :post post-kikka}
                  ::bar bar})})))

(app {:request-method :post, :uri "/kikka"})
; {:status 200, :body "post"}
```

```

`/Users/ovistoica/workspace/reitit/doc/advanced/route_validation.md`:

```md
# Route validation

Namespace `reitit.spec` contains [clojure.spec](https://clojure.org/about/spec) definitions for raw-routes, routes, router and router options.

## Example

```clj
(require '[clojure.spec.alpha :as s])
(require '[reitit.spec :as spec])

(def routes-from-db
  ["tenant1" ::tenant1])

(s/valid? ::spec/raw-routes routes-from-db)
; false

(s/explain ::spec/raw-routes routes-from-db)
; In: [0] val: "tenant1" fails spec: :reitit.spec/path at: [:route :path] predicate: (or (blank? %) (starts-with? % "/"))
; In: [0] val: "tenant1" fails spec: :reitit.spec/raw-route at: [:routes] predicate: (cat :path :reitit.spec/path :arg (? :reitit.spec/arg) :childs (* (and (nilable :reitit.spec/raw-route))))
; In: [1] val: :user/tenant1 fails spec: :reitit.spec/raw-route at: [:routes] predicate: (cat :path :reitit.spec/path :arg (? :reitit.spec/arg) :childs (* (and (nilable :reitit.spec/raw-route))))
; :clojure.spec.alpha/spec  :reitit.spec/raw-routes
; :clojure.spec.alpha/value  ["tenant1" :user/tenant1]
```

## At development time

`reitit.core/router` can be instrumented and use a tool like [expound](https://github.com/bhb/expound) to pretty-print the spec problems.

First add a `:dev` dependency to:

```clj
[expound "0.4.0"] ; or higher
```

Some bootstrapping:

```clj
(require '[clojure.spec.test.alpha :as stest])
(require '[expound.alpha :as expound])
(require '[clojure.spec.alpha :as s])
(require '[reitit.spec])

(stest/instrument `reitit/router)
(set! s/*explain-out* expound/printer)
```

And we are ready to go:

```clj
(require '[reitit.core :as r])

(r/router
  ["/api"
   ["/public"
    ["/ping"]
    ["pong"]]])

; CompilerException clojure.lang.ExceptionInfo: Call to #'reitit.core/router did not conform to spec:
;
; -- Spec failed --------------------
;
; Function arguments
;
; (["/api" ...])
;   ^^^^^^
;
;     should satisfy
;
; (clojure.spec.alpha/cat
;   :path
;   :reitit.spec/path
;   :arg
;   (clojure.spec.alpha/? :reitit.spec/arg)
;   :childs
;   (clojure.spec.alpha/*
;     (clojure.spec.alpha/and
;       (clojure.spec.alpha/nilable :reitit.spec/raw-route))))
;
; or
;
; (clojure.spec.alpha/cat
;   :path
;   :reitit.spec/path
;   :arg
;   (clojure.spec.alpha/? :reitit.spec/arg)
;   :childs
;   (clojure.spec.alpha/*
;     (clojure.spec.alpha/and
;       (clojure.spec.alpha/nilable :reitit.spec/raw-route))))
;
; -- Relevant specs -------
;
; :reitit.spec/raw-route:
; (clojure.spec.alpha/cat
;   :path
;   :reitit.spec/path
;   :arg
;   (clojure.spec.alpha/? :reitit.spec/arg)
;   :childs
;   (clojure.spec.alpha/*
;     (clojure.spec.alpha/and
;       (clojure.spec.alpha/nilable :reitit.spec/raw-route))))
; :reitit.spec/raw-routes:
; (clojure.spec.alpha/or
;   :route
;   :reitit.spec/raw-route
;   :routes
;   (clojure.spec.alpha/coll-of :reitit.spec/raw-route :into []))
;
; -- Spec failed --------------------
;
; Function arguments
;
; ([... [... ... ["pong"]]])
;                 ^^^^^^
;
;     should satisfy
;
; (fn
;   [%]
;   (or
;     (clojure.string/blank? %)
;     (clojure.string/starts-with? % "/")))
;
; or
;
; (fn
;   [%]
;   (or
;     (clojure.string/blank? %)
;     (clojure.string/starts-with? % "/")))
;
; -- Relevant specs -------
;
; :reitit.spec/path:
; (clojure.spec.alpha/and
;   clojure.core/string?
;   (clojure.core/fn
;     [%]
;     (clojure.core/or
;       (clojure.string/blank? %)
;       (clojure.string/starts-with? % "/"))))
; :reitit.spec/raw-route:
; (clojure.spec.alpha/cat
;   :path
;   :reitit.spec/path
;   :arg
;   (clojure.spec.alpha/? :reitit.spec/arg)
;   :childs
;   (clojure.spec.alpha/*
;     (clojure.spec.alpha/and
;       (clojure.spec.alpha/nilable :reitit.spec/raw-route))))
; :reitit.spec/raw-routes:
; (clojure.spec.alpha/or
;   :route
;   :reitit.spec/raw-route
;   :routes
;   (clojure.spec.alpha/coll-of :reitit.spec/raw-route :into []))
;
; -------------------------
; Detected 2 errors
```

```

`/Users/ovistoica/workspace/reitit/doc/advanced/configuring_routers.md`:

```md
# Configuring Routers

Routers can be configured via options. The following options are available for the `reitit.core/router`:

| key             | description
|-----------------|-------------
| `:path`         | Base-path for routes
| `:routes`       | Initial resolved routes (default `[]`)
| `:data`         | Initial route data (default `{}`)
| `:spec`         | clojure.spec definition for a route data, see `reitit.spec` on how to use this
| `:syntax`       | Path-parameter syntax as keyword or set of keywords (default #{:bracket :colon})
| `:expand`       | Function of `arg opts => data` to expand route arg to route data (default `reitit.core/expand`)
| `:coerce`       | Function of `route opts => route` to coerce resolved route, can throw or return `nil`
| `:compile`      | Function of `route opts => result` to compile a route handler
| `:validate`     | Function of `routes opts => ()` to validate route (data) via side-effects
| `:conflicts`    | Function of `{route #{route}} => ()` to handle conflicting routes
| `:exception`    | Function of `Exception => Exception ` to handle creation time exceptions (default `reitit.exception/exception`)
| `:meta-merge`   | Function of `left right => merged` to merge route-data (default `meta-merge.core/meta-merge`)
| `:update-paths` | Sequence of Vectors with elements `update-path` and `function`, used to preprocess route data
| `:router`       | Function of `routes opts => router` to override the actual router implementation



```

`/Users/ovistoica/workspace/reitit/doc/advanced/composing_routers.md`:

```md
# Composing Routers

Data-driven approach in `reitit` allows us to compose routes, route data, route specs, middleware and interceptors chains. We can compose routers too. This is needed to achieve dynamic routing like in [Compojure](https://github.com/weavejester/compojure).

## Immutability

Once a router is created, the routing tree is immutable and cannot be changed. To change the routing, we need to create a new router with changed routes and/or options. For this, the `Router` protocol exposes it's resolved routes via `r/routes` and options via `r/options`.

## Adding routes

Let's create a router:

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    [["/foo" ::foo]
     ["/bar/:id" ::bar]]))
```

We can query the resolved routes and options:

```clj
(r/routes router)
;[["/foo" {:name :user/foo}]
; ["/bar/:id" {:name :user/bar}]]

(r/options router)
;{:lookup #object[...]
; :expand #object[...]
; :coerce #object[...]
; :compile #object[...]
; :conflicts #object[...]}
```

Let's add a helper function to create a new router with extra routes:

```clj
(defn add-routes [router routes]
  (r/router
    (into (r/routes router) routes)
    (r/options router)))
```

We can now create a new router with extra routes:

```clj
(def router2
  (add-routes
    router
    [["/baz/:id/:subid" ::baz]]))

(r/routes router2)
;[["/foo" {:name :user/foo}]
; ["/bar/:id" {:name :user/bar}]
; ["/baz/:id/:subid" {:name :user/baz}]]
```

The original router was not changed:

```clj
(r/routes router)
;[["/foo" {:name :user/foo}]
; ["/bar/:id" {:name :user/bar}]]
```

When a new router is created, all rules are applied, including the conflict resolution:

```clj
(add-routes
  router2
  [["/:this/should/:fail" ::fail]])
;CompilerException clojure.lang.ExceptionInfo: Router contains conflicting route paths:
;
;   /baz/:id/:subid
;-> /:this/should/:fail
```

## Merging routers

Let's create a helper function to merge routers:

```clj
(defn merge-routers [& routers]
  (r/router
    (apply merge (map r/routes routers))
    (apply merge (map r/options routers))))
```

We can now merge multiple routers into one:

```clj
(def router
  (merge-routers
    (r/router ["/route1" ::route1])
    (r/router ["/route2" ::route2])
    (r/router ["/route3" ::route3])))

(r/routes router)
;[["/route1" {:name :user/route1}]
; ["/route2" {:name :user/route2}]
; ["/route3" {:name :user/route3}]]
```

## Nesting routers

Routers can be nested using the catch-all parameter.

Here's a router with deeply nested routers under a `:router` key in the route data:

```clj
(def router
  (r/router
    [["/ping" :ping]
     ["/olipa/*" {:name :olipa
                  :router (r/router
                            [["/olut" :olut]
                             ["/makkara" :makkara]
                             ["/kerran/*" {:name :kerran
                                           :router (r/router
                                                     [["/avaruus" :avaruus]
                                                      ["/ihminen" :ihminen]])}]])}]]))
```

Matching by path:

```clj
(r/match-by-path router "/olipa/kerran/iso/kala")
;#Match{:template "/olipa/*"
;       :data {:name :olipa
;              :router #object[reitit.core$mixed_router]}
;       :result nil
;       :path-params {: "kerran/iso/kala"}
;       :path "/olipa/iso/kala"}
```

That didn't work as we wanted, as the nested routers don't have such a route. The core routing doesn't understand anything the `:router` key, so it only matched against the top-level router, which gave a match for the catch-all path.

As the `Match` contains all the route data, we can create a new matching function that understands the `:router` key. Below is a function that does recursive matching using the subrouters. It returns either `nil` or a vector of matches.

```clj
(require '[clojure.string :as str])

(defn recursive-match-by-path [router path]
  (when-let [match (r/match-by-path router path)]
    (if-let [subrouter (-> match :data :router)]
      (let [subpath (subs path (str/last-index-of (:template match) "/"))]
        (when-let [submatch (recursive-match-by-path subrouter subpath)]
          (cons match submatch)))
      (list match))))
```

With invalid nested path we get now `nil` as expected:

```clj
(recursive-match-by-path router "/olipa/kerran/iso/kala")
; nil
```

With valid path we get all the nested matches:

```clj
(recursive-match-by-path router "/olipa/kerran/avaruus")
;[#reitit.core.Match{:template "/olipa/*"
;                    :data {:name :olipa
;                           :router #object[reitit.core$mixed_router]}
;                    :result nil
;                    :path-params {: "kerran/avaruus"}
;                    :path "/olipa/kerran/avaruus"}
; #reitit.core.Match{:template "/kerran/*"
;                    :data {:name :kerran
;                           :router #object[reitit.core$lookup_router]}
;                    :result nil
;                    :path-params {: "avaruus"}
;                    :path "/kerran/avaruus"}
; #reitit.core.Match{:template "/avaruus"
;                    :data {:name :avaruus}
;                    :result nil
;                    :path-params {}
;                    :path "/avaruus"}]
```

Let's create a helper to get only the route names for matches:

```clj
(defn name-path [router path]
  (some->> (recursive-match-by-path router path)
           (mapv (comp :name :data))))

(name-path router "/olipa/kerran/avaruus")
; [:olipa :kerran :avaruus]
```

So, we can nest routers, but why would we do that?

## Dynamic routing

In all the examples above, the routers were created ahead of time, making the whole route tree effectively static.  To have more dynamic routing, we can use router references allowing the router to be swapped over time. We can also create fully dynamic routers where the router is re-created for each request. Let's walk through both cases.

First, we need to modify our matching function to support router references:

```clj
(defn- << [x]
  (if (instance? clojure.lang.IDeref x)
    (deref x) x))

(defn recursive-match-by-path [router path]
  (when-let [match (r/match-by-path (<< router) path)]
    (if-let [subrouter (-> match :data :router <<)]
      (let [subpath (subs path (str/last-index-of (:template match) "/"))]
        (when-let [submatch (recursive-match-by-path subrouter subpath)]
          (cons match submatch)))
      (list match))))
```

Then, we need some routers.

First, a reference to a router that can be updated on background, for example when a new entry in inserted into a database. We'll wrap the router into a `atom`:

```clj
(def beer-router
  (atom
    (r/router
      [["/lager" :lager]])))
```

Second, a reference to router, which is re-created on each routing request:

```clj
(def dynamic-router
  (reify clojure.lang.IDeref
    (deref [_]
      (r/router
        ["/duo" (keyword (str "duo" (rand-int 100)))]))))
```

We can compose the routers into a system-level static root router:

```clj
(def router
  (r/router
    [["/gin/napue" :napue]
     ["/ciders/*" :ciders]
     ["/beers/*" {:name :beers
                  :router beer-router}]
     ["/dynamic/*" {:name :dynamic
                    :router dynamic-router}]]))
```

Matching root routes:

```clj
(name-path router "/vodka/russian")
; nil

(name-path router "/gin/napue")
; [:napue]
```

Matching (nested) beer routes:

```clj
(name-path router "/beers/lager")
; [:beers :lager]

(name-path router "/beers/saison")
; nil
```

No saison!? Let's add the route:

```clj
(swap! beer-router add-routes [["/saison" :saison]])
```

There we have it:

```clj
(name-path router "/beers/saison")
; [:beers :saison]
```

We can't add conflicting routes:

```clj
(swap! beer-router add-routes [["/saison" :saison]])
;CompilerException clojure.lang.ExceptionInfo: Router contains conflicting route paths:
;
;   /saison
;-> /saison
```

The dynamic routes are re-created on every request:

```clj
(name-path router "/dynamic/duo")
; [:dynamic :duo71]

(name-path router "/dynamic/duo")
; [:dynamic :duo55]
```

### Performance

With nested routers, instead of having to do just one route match, matching is recursive, which adds a small cost. All nested routers need to be of type catch-all at top-level, which is order of magnitude slower than fully static routes. Dynamic routes are the slowest ones, at least two orders of magnitude slower, as the router needs to be recreated for each request.

A quick benchmark on the recursive lookups:

| path             | time    | type
|------------------|---------|-----------------------
| `/gin/napue`     | 40ns    | static
| `/ciders/weston` | 440ns   | catch-all
| `/beers/saison`  | 600ns   | catch-all + static
| `/dynamic/duo`   | 12000ns | catch-all + dynamic

The non-recursive lookup for `/gin/napue` is around 23ns.

Comparing the dynamic routing performance with Compojure:

```clj
(require '[compojure.core :refer [context])

(def app
  (context "/dynamic" [] (constantly :duo)))

(app {:uri "/dynamic/duo" :request-method :get})
; :duo
```

| path             | time    | type
|------------------|---------|-----------------------
| `/dynamic/duo`   | 20000ns | compojure

Can we make the nester routing faster? Sure. We could use the Router `:compile` hook to compile the nested routers for better performance. We could also allow router creation rules to be disabled, to get the dynamic routing much faster.

### When to use nested routers?

Nesting routers is not trivial and because of that, should be avoided. For dynamic (request-time) route generation, it's the only choice. For other cases, nested routes are most likely a better option.

Let's re-create the previous example with normal route nesting/composition.

A helper to the root router:

```clj
(defn create-router [beers]
  (r/router
    [["/gin/napue" :napue]
     ["/ciders/*" :ciders]
     ["/beers" (for [beer beers]
                 [(str "/" beer) (keyword "beer" beer)])]
     ["/dynamic/*" {:name :dynamic
                    :router dynamic-router}]]))
```

New new root router *reference* and a helper to reset it:

```clj
(def router
  (atom (create-router nil)))

(defn reset-router! [beers]
  (reset! router (create-router beers)))
```

The routing tree:

```clj
(r/routes @router)
;[["/gin/napue" {:name :napue}]
; ["/ciders/*" {:name :ciders}]
; ["/dynamic/*" {:name :dynamic,
;                :router #object[user$reify__24359]}]]
```

Let's reset the router with some beers:

```clj
(reset-router! ["lager" "sahti" "bock"])
```

We can see that the beer routes are now embedded into the core router:

```clj
(r/routes @router)
;[["/gin/napue" {:name :napue}]
; ["/ciders/*" {:name :ciders}]
; ["/beers/lager" {:name :beer/lager}]
; ["/beers/sahti" {:name :beer/sahti}]
; ["/beers/bock" {:name :beer/bock}]
; ["/dynamic/*" {:name :dynamic,
;                :router #object[user$reify__24359]}]]
```

And the routing works:

```clj
(name-path @router "/beers/sahti")
;[:beer/sahti]
```

All the beer-routes now match in constant time.

| path            | time    | type
|-----------------|---------|-----------------------
| `/beers/sahti`  | 40ns    | static

### Wrapping a swappable ring handler

In order for a ring handler to be recomposed, we can wrap it into a handler that dereferences it on request.

```clj
(defn deref-handler [rf]
  (fn
    ([request] (@rf request))
    ([request respond raise] (@rf request respond raise))))
```

A simplified beer router version that creates a ring-handler.

```clj
(defn create-ring-handler [beers]
  (ring/ring-handler
   (ring/router
    [["/beers"
      (when (seq beers)
        (for [beer beers]
          [(str "/" beer)
           {:get (fn [_] {:status 200 :body beer})}]))]])))

(def ring-handler
  (atom (create-ring-handler nil)))

(defn reset-router! [beers]
  (reset! ring-handler (create-ring-handler beers)))
```

We don't have any matching routes yet.

```clj
((deref-handler ring-handler) {:request-method :get :uri "/beers/lager"})
; nil
```

But we can add them later.

```clj
(reset-router! ["lager"])
((deref-handler ring-handler) {:request-method :get :uri "/beers/lager"})
; {:status 200, :body "lager"}
```

## TODO

* maybe create a `recursive-router` into a separate ns with all `Router` functions implemented correctly? maybe not...
* add `reitit.core/merge-routes` to effectively merge routes with route data


```

`/Users/ovistoica/workspace/reitit/doc/frontend/basics.md`:

```md
# Frontend basics

Reitit frontend integration is built from multiple layers:

- Core functions with some additional browser oriented features
- [Browser integration](./browser.md) for attaching Reitit to hash-change or HTML
history events
- Stateful wrapper for easy use of history integration
- Optional [controller extension](./controllers.md)

You likely won't use `reitit.frontend` directly in your apps and instead you
will use the API documented in the browser integration docs, which wraps these
lower level functions.

## Core functions

`reitit.frontend` provides some useful functions wrapping core functions:

`match-by-path` version which parses a URI using JavaScript, including
query-string, and also [coerces the parameters](../coercion/coercion.md).
Coerced parameters are stored in match `:parameters` property. If coercion
is not enabled, the original parameters are stored in the same property,
to allow the same code to read parameters regardless if coercion is
enabled.

`router` which compiles coercers by default.

`match-by-name` and `match-by-name!` with optional `path-paramers` and
logging errors to `console.warn` instead of throwing errors to prevent
React breaking due to errors. These can also [encode query-parameters](./coercion.md)
using schema from match data.

## Next

[Browser integration](./browser.md)

```

`/Users/ovistoica/workspace/reitit/doc/frontend/coercion.md`:

```md
# Frontend coercion

The Reitit frontend leverages [coercion](../coercion/coercion.md) for path,
query, and fragment parameters. The coercion uses the input schema defined
in the match data under `:parameters`.

## Behavior of Coercion

1. **Route Matching**
   When matching a route from a path, the resulting match will include the
   coerced values (if coercion is enabled) under `:parameters`. If coercion is
   disabled, the parsed string values are stored in the same location.
   The original un-coerced values are always available under `:path-params`,
   `:query-params`, and `:fragment` (a single string).

2. **Creating Links and Navigating**
   When generating a URL (`href`) or navigating (`push-state`, `replace-state`, `navigate`)
   to a route, coercion can be
   used to encode query-parameter values into strings. This happens before
   Reitit performs basic URL encoding on the values. This feature is
   especially useful for handling the encoding of specific types, such as
   keywords or dates, into strings.

3. **Updating current query parameters**
  When using `set-query` to modify current query parameters, Reitit frontend
  first tries to find a match for the current path so the match can be used to
  first decode query parameters and then to encode them. If the current path
  doesn't match the routing tree, `set-query` keeps all the query parameter
  values as strings.

## Notes

- **Value Encoding Support**: Only Malli supports value encoding.
- **Limitations**: Path parameters and fragment values are not encoded using
  the match schema.

## Example

```cljs
(def router (r/router ["/"
                       ["" ::frontpage]
                       ["bar"
                        {:name ::bar
                         :coercion rcm/coercion
                         :parameters {:query [:map
                                              [:q {:optional true}
                                               [:keyword
                                                {:decode/string (fn [s] (keyword (subs s 2)))
                                                 :encode/string (fn [k] (str "__" (name k)))}]]]}}]]))

(rfe/href ::bar {} {:q :hello})
;; Result "/bar?q=__hello", the :q value is first encoded

(rfe/push-state ::bar {} {:q :world})
;; Result "/bar?q=__world"
;; The current match will contain both the original value and parsed & decoded parameters:
;; {:query-params {:q "__world"}
;;  :parameters {:query {:q :world}}}
```

```

`/Users/ovistoica/workspace/reitit/doc/frontend/browser.md`:

```md
# Frontend browser integration

Reitit includes two browser history integrations.

Main functions are `navigate` and `set-query`. Navigate is used to navigate
to named routes, and the options parameter can be used to control all
parameters and if `pushState` or `replaceState` should be used to control
browser history stack. The `set-query` function can be used to change
or modify query parameters for the current route, it takes either map of
new query params or function from old params to the new params.

There are also secondary functions following HTML5 History API:
`push-state` to navigate to new route adding entry to the history and
`replace-state` to change route without leaving previous entry in browser history.

See [coercion notes](./coercion.md) to see how frontend route parameters
can be decoded and encoded.

## Fragment router

Fragment is simple integration which stores the current route in URL fragment,
i.e. after `#`. This means the route is never part of the request URI and
server will always know which file to return (`index.html`).

## HTML5 router

HTML5 History API can be used to modify the URL in browser without making
request to the server. This means the URL will look normal, but the downside is
that the server must respond to all routes with correct file (`index.html`).
Check examples for simple Ring handler example.

### Anchor click handling

HTML5 History router will handle click events on anchors where the href
matches the route tree (and other [rules](../../modules/reitit-frontend/src/reitit/frontend/history.cljs#L84-L98)).
If you have need to control this logic, for example to handle some
anchor clicks where the href matches route tree normally (i.e. browser load)
you can provide `:ignore-anchor-click?` function to add your own logic to
event handling:

```clj
(rfe/start!
  router
  on-navigate-fn
  {:use-fragment false
   :ignore-anchor-click? (fn [router e el uri]
                           ;; Add additional check on top of the default checks
                           (and (rfh/ignore-anchor-click? router e el uri)
                                (not= "false" (gobj/get (.-dataset el) "reititHandleClick"))))})

;; Use data-reitit-handle-click to disable Reitit anchor handling
[:a
 {:href (rfe/href ::about)
  :data-reitit-handle-click false}
 "About"]
```

## Easy

Reitit frontend routers require storing the state somewhere and passing it to
all the calls. Wrapper `reitit.frontend.easy` is provided which manages
a router instance and passes the instance to all calls. This should
allow easy use in most applications, as browser anyway can only have single
event handler for page change events.

## History manipulation

Reitit doesn't include functions to manipulate the history stack, i.e.,
go back or forwards, but calling History API functions directly should work:

```
(.go js/window.history -1)
;; or
(.back js/window.history)
```

```

`/Users/ovistoica/workspace/reitit/doc/frontend/controllers.md`:

```md
# Controllers

* https://github.com/metosin/reitit/tree/master/examples/frontend-controllers

Controllers run code when a route is entered and left. This can be useful to:

- Load resources
- Update application state

## How controllers work

A controller map can contain these properties:

* `identity` function which takes a Match and returns an arbitrary value,
* or `parameters` value, which declares which parameters should affect
controller identity
* `start` & `stop` functions, which are called with controller identity

When you navigate to a route that has a controller, controller identity
is first resolved by using `parameters` declaration, or by calling `identity` function,
or if neither is set, the identity is `nil`. Next, the controller
is initialized by calling `start` with the controller identity value.
When you exit that route, `stop` is called with the last controller identity value.

If you navigate to the same route with different match, identity gets
resolved again. If the identity changes from the previous value, controller
is reinitialized: `stop` and `start` get called again.

You can add controllers to a route by adding them to the route data in the
`:controllers` vector. For example:

```cljs
["/item/:id"
 {:controllers [{:parameters {:path [:id]}
                 :start  (fn [parameters] (js/console.log :start (-> parameters :path :id)))
                 :stop   (fn [parameters] (js/console.log :stop (-> parameters :path :id)))}]}]
```

You can leave out `start` or `stop` if you do not need both of them.

## Enabling controllers

You need to
call
[`reitit.frontend.controllers/apply-controllers`](https://cljdoc.org/d/metosin/reitit-frontend/CURRENT/api/reitit.frontend.controllers#apply-controllers) whenever
the URL changes. You can call it from the `on-navigate` callback of
`reitit.frontend.easy`:

```cljs
(ns frontend.core
  (:require [reitit.frontend.easy :as rfe]
            [reitit.frontend.controllers :as rfc]))

(defonce match-a (atom nil))

(def routes
  ["/" ...])

(defn init! []
  (rfe/start!
    routes
    (fn [new-match]
      (swap! match-a
        (fn [old-match]
          (when new-match
            (assoc new-match
              :controllers (rfc/apply-controllers (:controllers old-match) new-match))))))))
```

See also [the full example](https://github.com/metosin/reitit/tree/master/examples/frontend-controllers).

## Nested controllers

When you nest routes in the route tree, the controllers get concatenated when
route data is merged. Consider this route tree:

```cljs
["/" {:controllers [{:start (fn [_] (js/console.log "root start"))}]}
 ["/item/:id"
  {:controllers [{:parameters {:path [:id]}
                  :start (fn [parameters] (js/console.log "item start" (-> parameters :path :id)))
                  :stop  (fn [parameters] (js/console.log "item stop" (-> parameters :path :id)))}]}]]

```

* When you navigate to any route at all, the root controller gets started.
* If you navigate to `/item/something`, the root controller gets started first
  and then the item controller gets started.
* If you then navigate from `/item/something` to `/item/something-else`, first
  the item controller gets stopped with parameter `something` and then it gets
  started with the parameter `something-else`. The root controller stays on the
  whole time since its parameters do not change.

## Tips

### Authentication

Controllers can be used to load resources from a server. If and when your
API requires authentication you will need to implement logic to prevent controllers
trying to do requests if user isn't authenticated yet.

#### Run controllers and check authentication

If you have both unauthenticated and authenticated resources, you can
run the controllers always and then check the authentication status
on controller code, or on the code called from controllers (e.g. re-frame event
handler).

#### Disable controllers until user is authenticated

If all your resources require authentication an easy way to prevent bad
requests is to enable controllers only after authentication is done.
To do this you can check authentication status and call `apply-controllers`
only after authentication is done (also remember to manually call `apply-controllers`
with current `match` when authentication is done). Or if no navigation is possible
before authentication is done, you can start the router only after
authentication is done.

## Alternatives

Similar solution could be used to describe required resources as data (maybe
even GraphQL query) per route, and then have code automatically load
missing resources.

## Controllers elsewhere

* [Controllers in Keechma](https://keechma.com/guides/controllers/)

```

`/Users/ovistoica/workspace/reitit/doc/SUMMARY.md`:

```md
# Summary

## Introduction

* [Introduction](README.md)

## Basics

* [Route Syntax](basics/route_syntax.md)
* [Router](basics/router.md)
* [Path-based Routing](basics/path_based_routing.md)
* [Name-based Routing](basics/name_based_routing.md)
* [Route Data](basics/route_data.md)
* [Route Data Validation](basics/route_data_validation.md)
* [Route Conflicts](basics/route_conflicts.md)
* [Error Messages](basics/error_messages.md)

## Coercion

* [Coercion Explained](coercion/coercion.md)
* [Plumatic Schema](coercion/schema_coercion.md)
* [Clojure.spec](coercion/clojure_spec_coercion.md)
* [Data-specs](coercion/data_spec_coercion.md)
* [Malli](coercion/malli_coercion.md)

## Ring

* [Ring Router](ring/ring.md)
* [Reverse-routing](ring/reverse_routing.md)
* [Default handler](ring/default_handler.md)
* [Slash handler](ring/slash_handler.md)
* [Static Resources](ring/static.md)
* [Dynamic Extensions](ring/dynamic_extensions.md)
* [Data-driven Middleware](ring/data_driven_middleware.md)
* [Transforming Middleware Chain](ring/transforming_middleware_chain.md)
* [Middleware Registry](ring/middleware_registry.md)
* [Exception Handling with Ring](ring/exceptions.md)
* [Default Middleware](ring/default_middleware.md)
* [Content Negotiation](ring/content_negotiation.md)
* [Pluggable Coercion](ring/coercion.md)
* [Route Data Validation](ring/route_data_validation.md)
* [Compiling Middleware](ring/compiling_middleware.md)
* [Swagger Support](ring/swagger.md)
* [OpenAPI Support](ring/openapi.md)
* [RESTful form methods](ring/RESTful_form_methods.md)

## HTTP

* [Interceptors](http/interceptors.md)
* [Pedestal](http/pedestal.md)
* [Sieppari](http/sieppari.md)
* [Default Interceptors](http/default_interceptors.md)
* [Transforming Interceptor Chain](http/transforming_interceptor_chain.md)

## Frontend

* [Basics](frontend/basics.md)
* [Browser integration](frontend/browser.md)
* [Controllers](frontend/controllers.md)

## Advanced

* [Configuring Routers](advanced/configuring_routers.md)
* [Composing Routers](advanced/composing_routers.md)
* [Different Routers](advanced/different_routers.md)
* [Route Validation](advanced/route_validation.md)
* [Dev Workflow](advanced/dev_workflow.md)
* [Shared Routes](advanced/shared_routes.md)

## Misc

* [Performance](performance.md)
* [Development Instructions](development.md)
* [FAQ](faq.md)

```

`/Users/ovistoica/workspace/reitit/doc/cljdoc.edn`:

```edn
{:cljdoc/include-namespaces-from-dependencies
 [metosin/reitit
  metosin/reitit-core
  metosin/reitit-dev
  metosin/reitit-ring
  metosin/reitit-http
  metosin/reitit-middleware
  metosin/reitit-interceptors
  metosin/reitit-spec
  metosin/reitit-schema
  metosin/reitit-swagger
  metosin/reitit-swagger-ui
  metosin/reitit-frontend
  metosin/reitit-sieppari
  metosin/reitit-pedestal
  fi.metosin/reitit-openapi]
 :cljdoc.doc/tree
 [["Introduction" {:file "doc/README.md"}]
  ["Basics" {}
   ["Route Syntax" {:file "doc/basics/route_syntax.md"}]
   ["Router" {:file "doc/basics/router.md"}]
   ["Path-based Routing" {:file "doc/basics/path_based_routing.md"}]
   ["Name-based Routing" {:file "doc/basics/name_based_routing.md"}]
   ["Route Data" {:file "doc/basics/route_data.md"}]
   ["Route Data Validation" {:file "doc/basics/route_data_validation.md"}]
   ["Route Conflicts" {:file "doc/basics/route_conflicts.md"}]
   ["Error Messages" {:file "doc/basics/error_messages.md"}]]
  ["Coercion" {}
   ["Coercion Explained" {:file "doc/coercion/coercion.md"}]
   ["Plumatic Schema" {:file "doc/coercion/schema_coercion.md"}]
   ["Clojure.spec" {:file "doc/coercion/clojure_spec_coercion.md"}]
   ["Data-specs" {:file "doc/coercion/data_spec_coercion.md"}]
   ["Malli" {:file "doc/coercion/malli_coercion.md"}]]
  ["Ring" {}
   ["Ring Router" {:file "doc/ring/ring.md"}]
   ["Reverse-routing" {:file "doc/ring/reverse_routing.md"}]
   ["Default handler" {:file "doc/ring/default_handler.md"}]
   ["Slash handler" {:file "doc/ring/slash_handler.md"}]
   ["Static Resources" {:file "doc/ring/static.md"}]
   ["Dynamic Extensions" {:file "doc/ring/dynamic_extensions.md"}]
   ["Data-driven Middleware" {:file "doc/ring/data_driven_middleware.md"}]
   ["Transforming Middleware Chain" {:file "doc/ring/transforming_middleware_chain.md"}]
   ["Middleware Registry" {:file "doc/ring/middleware_registry.md"}]
   ["Exception Handling with Ring" {:file "doc/ring/exceptions.md"}]
   ["Default Middleware" {:file "doc/ring/default_middleware.md"}]
   ["Content Negotiation" {:file "doc/ring/content_negotiation.md"}]
   ["Pluggable Coercion" {:file "doc/ring/coercion.md"}]
   ["Route Data Validation" {:file "doc/ring/route_data_validation.md"}]
   ["Compiling Middleware" {:file "doc/ring/compiling_middleware.md"}]
   ["Swagger Support" {:file "doc/ring/swagger.md"}]
   ["OpenAPI Support" {:file "doc/ring/openapi.md"}]
   ["RESTful form methods" {:file "doc/ring/RESTful_form_methods.md"}]]
  ["HTTP" {}
   ["Interceptors" {:file "doc/http/interceptors.md"}]
   ["Pedestal" {:file "doc/http/pedestal.md"}]
   ["Sieppari" {:file "doc/http/sieppari.md"}]
   ["Default Interceptors" {:file "doc/http/default_interceptors.md"}]
   ["Transforming Interceptor Chain" {:file "doc/http/transforming_interceptor_chain.md"}]]
  ["Frontend" {}
   ["Basics" {:file "doc/frontend/basics.md"}]
   ["Browser integration" {:file "doc/frontend/browser.md"}]
   ["Controllers" {:file "doc/frontend/controllers.md"}]]
  ["Advanced" {}
   ["Configuring Routers" {:file "doc/advanced/configuring_routers.md"}]
   ["Composing Routers" {:file "doc/advanced/composing_routers.md"}]
   ["Different Routers" {:file "doc/advanced/different_routers.md"}]
   ["Route Validation" {:file "doc/advanced/route_validation.md"}]
   ["Dev Workflow" {:file "doc/advanced/dev_workflow.md"}]
   ["Shared Routes" {:file "doc/advanced/shared_routes.md"}]]
  ["Misc" {}
   ["Performance" {:file "doc/performance.md"}]
   ["Development Instructions" {:file "doc/development.md"}]
   ["FAQ" {:file "doc/faq.md"}]]]}

```

`/Users/ovistoica/workspace/reitit/doc/faq.md`:

```md
# Frequently Asked Questions

* [Why yet another routing library?](#why-yet-another-routing-library)
* [How can I contribute?](#how-can-i-contribute)
* [How does Reitit differ from Bidi?](#how-does-reitit-differ-from-bidi)
* [How does Reitit differ from Pedestal?](#how-does-reitit-differ-from-pedestal)
* [How does Reitit differ from Compojure?](#how-does-reitit-differ-from-compojure)
* [How do you pronounce "reitit"?](#how-do-you-pronounce-reitit)

### Why yet another routing library?

Routing and dispatching is in the core of most business apps, so we should have a great library to for it. There are already many good routing libs for Clojure, but we felt none was perfect. So, we took best parts of existing libs and added features that were missing: first-class composable route data, full route conflict resolution and pluggable coercion. Goal was to make a data-driven library that works, is fun to use and is really, really fast.

### How can I contribute?

You can join [#reitit](https://clojurians.slack.com/messages/reitit/) channel in [Clojurians slack](http://clojurians.net/) to discuss things. Known roadmap is mostly written in [issues](https://github.com/metosin/reitit/issues).

### How does Reitit differ from Bidi?

[Bidi](https://github.com/juxt/bidi) is an great and proven library for ClojureScript and we have been using it in many of our frontend projects. Both Reitit and Bidi are data-driven, bi-directional and work with both Clojure & ClojureScript. Here are the main differences:

#### Route syntax

* Bidi supports multiple representations for route syntax, Reitit supports just one (simple) syntax.
* Bidi uses special (Clojure) syntax for route patterns while Reitit separates (human-readable) paths strings from route data - still exposing the machine-readable syntax for extensions.

Bidi:

```clj
(def routes
  ["/" [["auth/login" :auth/login]
        [["auth/recovery/token/" :token] :auth/recovery]
        ["workspace/" [[[:project-uuid "/" :page-uuid] :workspace/page]]]]])
```

Reitit:

```clj
(def routes
  [["/auth/login" :auth/login]
   ["/auth/recovery/token/:token" :auth/recovery]
   ["/workspace/:project-uuid/:page-uuid" :workspace/page]])
```

#### Features

* Bidi has extra features like route guards
* Reitit ships with composable route data, specs, full route conflict resolution and pluggable coercion.

#### Performance

* Bidi is not optimized for speed and thus, Reitit is [much faster](performance.md) than Bidi. From Bidi source:

```clj
;; Route compilation was only marginally effective and hard to
;; debug. When bidi matching takes in the order of 30 micro-seconds,
;; this is good enough in relation to the time taken to process the
;; overall request.
```

### How does Reitit differ from Pedestal?

[Pedestal](http://pedestal.io/) is an great and proven library and has had great influence in Reitit. Both Reitit and Pedestal are data-driven and provide bi-directional routing and fast. Here are the main differences:

#### ClojureScript

* Pedestal targets only Clojure, while Reitit works also with ClojureScript.

#### Route syntax

* Pedestal supports multiple representations for route syntax: terse, table and verbose. Reitit provides only one representation.
* Pedestal supports both maps or keyword-arguments in route data, in Reitit, it's all maps.

Pedestal:

```clj
["/api/ping" :get identity :route-name ::ping]
```

Reitit:

```clj
["/api/ping" {:get identity, :name ::ping}]
```

#### Features

* Pedestal supports route guards
* Pedestal supports interceptors (`reitit-http` module will support them too).
* Reitit ships with composable route data, specs, full route conflict resolution and pluggable coercion.
* In Pedestal, different routers [behave differently](https://github.com/pedestal/pedestal/issues/532), in Reitit, all work the same.

#### Performance

Reitit routing was originally based on Pedestal Routing an thus they same similar performance. For routing trees with both static and wildcard routes, Reitit is much faster thanks to it's `mixed-router` algorithm.

### How does Reitit differ from Compojure?

[Compojure](https://github.com/weavejester/compojure) is the most used routing library in Clojure. It's proven and awesome.

#### ClojureScript

* Compojure targets only Clojure, while Reitit works also with ClojureScript.

#### Route syntax

* Compojure uses routing functions and macros while reitit is all data
* Compojure allows easy destructuring of route params on mid-path
* Applying middleware for sub-paths is hacky on Compojure, `reitit-ring` resolves this with data-driven middleware

Compojure:

```clj
(defroutes routes
  (wrap-routes
    (context "/api" []
      (GET "/users/:id" [id :<< as-int]
        (ok (get-user id)))
      (POST "/pizza" []
        (wrap-log post-pizza-handler)))
    wrap-api :secure))
```

`reitit-ring` with `reitit-spec` module:

```clj
(def routes
  ["/api" {:middleware [[wrap-api :secure]]}
   ["/users/:id" {:get {:parameters {:path {:id int?}}}
                  :handler (fn [{:keys [parameters]}]
                             (ok (get-user (-> parameters :body :id))))}
    ["/pizza" {:post {:middleware [wrap-log]
                      :handler post-pizza-handler}]]])
```

#### Features

* Dynamic routing is trivial in Compojure, with reitit, some trickery is needed
* Reitit ships with composable route data, specs, full route conflict resolution and pluggable coercion.

#### Performance

Reitit is [much faster](performance.md) than Compojure.

### How do you pronounce "reitit"?

[Google Translate does a decent job pronouncing it](https://translate.google.com/#view=home&op=translate&sl=fi&tl=en&text=reitit) (click the speaker icon on the left). The English expression *rate it* is a good approximation.

```

`/Users/ovistoica/workspace/reitit/doc/performance.md`:

```md
# Performance

Reitit tries to be really, really fast.

![Opensensors perf test](images/opensensors.png)

### Rationale

* Multiple routing algorithms, chosen based on the route tree
* Route flattening and re-ordering
* Managed mutability over immutability
* Precompute/compile as much as possible (matches, middleware, interceptors, routes, path-parameter sets)
* Use abstractions that enable JVM optimizations
* Use small functions to enable JVM Inlining
* Use Java where needed
* Protocols over Multimethods
* Records over Maps
* Always be measuring
* Don't trust the (micro-)benchmarks

### Does routing performance matter?

Well, it depends. With small route trees, it might not. But, with large (real-life) route trees, difference between the fastest and the slowest tested libs can be two or three orders of magnitude. For busy sites it actually matters if you routing request takes 100 ns or 100 µs. A lot.

### TechEmpower Web Framework Benchmarks

Reitit + [jsonista](https://github.com/metosin/jsonista) + [pohjavirta](https://github.com/metosin/pohjavirta) is one of the fastest JSON api stacks in the tests. See full results [here](https://www.techempower.com/benchmarks/#section=test&runid=42f65a64-69b2-400d-b24e-20ecec9848bc&hw=ph&test=json).

![Tech](images/tfb_json.png)

### Tests

All perf tests are found in [the repo](https://github.com/metosin/reitit/tree/master/perf-test/clj/reitit) and have been run with the following setup:

```
;;
;; start repl with `lein perf repl`
;; perf measured with the following setup:
;;
;; Model Name:            MacBook Pro
;; Model Identifier:      MacBookPro11,3
;; Processor Name:        Intel Core i7
;; Processor Speed:       2,5 GHz
;; Number of Processors:  1
;; Total Number of Cores: 4
;; L2 Cache (per Core):   256 KB
;; L3 Cache:              6 MB
;; Memory:                16 GB
;;
```

**NOTE:** Tests are not scientific proof and may contain errors. You should always run the perf tests with your own (real-life) routing tables to get more accurate results for your use case. Also, if you have idea how to test things better, please let us know.

### Simple Example

The routing sample taken from [bide](https://github.com/funcool/bide) README:

```clj
(require '[reitit.core :as r])
(require '[criterium.core :as cc])

(def routes
  (r/router
    [["/auth/login" :auth/login]
     ["/auth/recovery/token/:token" :auth/recovery]
     ["/workspace/:project/:page" :workspace/page]]))

;; Execution time mean (per 1000) : 3.2 µs -> 312M ops/sec
(cc/quick-bench
  (dotimes [_ 1000]
    (r/match-by-path routes "/auth/login")))

;; Execution time mean (per 1000): 115 µs -> 8.7M ops/sec
(cc/quick-bench
  (dotimes [_ 1000]
    (r/match-by-path routes "/workspace/1/1")))
```

Based on the [perf tests](https://github.com/metosin/reitit/blob/master/perf-test/clj/reitit/bide_perf_test.clj), the first (static path) lookup is 300-500x faster and the second (wildcard path) lookup is 18-110x faster that the other tested routing libs (Ataraxy, Bidi, Compojure and Pedestal).

But, the example is too simple for any real benchmark. Also, some of the libraries always match on the `:request-method` too and by doing so, do more work than just match by path. Compojure does most work also by invoking the handler.

So, we need to test something more realistic.

### RESTful apis

To get better view on the real life routing performance, there is [test](https://github.com/metosin/reitit/blob/master/perf-test/clj/reitit/opensensors_perf_test.clj) of a mid-size rest(ish) http api with 50+ routes, having a lot of path parameters. The route definitions are pulled off from the [OpenSensors](https://opensensors.io/) swagger definitions.

Thanks to the snappy [Wildcard  Trie](https://github.com/metosin/reitit/blob/master/modules/reitit-core/java-src/reitit/Trie.java) (a modification of [Radix Tree](https://en.wikipedia.org/wiki/Radix_tree)), `reitit-ring` is fastest here. [Calfpath](https://github.com/kumarshantanu/calfpath) and [Pedestal](https://github.com/pedestal/pedestal) are also quite fast.

![Opensensors perf](images/opensensors.png)

### CQRS apis

Another real-life [test scenario](https://github.com/metosin/reitit/blob/master/perf-test/clj/reitit/lupapiste_perf_test.clj) is a [CQRS](https://martinfowler.com/bliki/CQRS.html) style route tree, where all the paths are static, e.g. `/api/command/add-order`. The 300 route definitions are pulled out from [Lupapiste](https://github.com/lupapiste/lupapiste).

Both `reitit-ring` and Pedestal shine in this test, thanks to the fast lookup-routers. On average, they are **two** and on best case, **three orders of magnitude faster** than the other tested libs. Ataraxy failed this test on `Method code too large!` error.

![lupapiste perf](images/lupapiste.png)

**NOTE**: in real life, there are usually always also wild-card routes present. In this case, Pedestal would fallback from lookup-router to the prefix-tree router, which is order of magnitude slower (30x in this test). Reitit would handle this nicely thanks to it's `:mixed-router`: all static routes would still be served with `:lookup-router`, just the wildcard routes with `:segment-tree`. The performance would not notably degrade.

### Path conflicts

**TODO**

### Why measure?

The reitit routing perf is measured to get an internal baseline to optimize against. We also want to ensure that new features don't regress the performance. Perf tests should be run in a stable CI environment. Help welcome!

### Looking out of the box

A quick poke to [the fast routers in Go](https://github.com/julienschmidt/go-http-routing-benchmark) indicates that reitit is less than 50% slower than the fastest routers in Go. Which is kinda awesome.

### Faster!

By default, `reitit.ring/router`, `reitit.http/router` and `reitit.http/routing-interceptor` inject both `Match` and `Router` into the request. You can remove the injections setting options `:inject-match?` and `:inject-router?` to `false`. This saves some tens of nanos (with the hw described above).

```clj
(require '[reitit.ring :as ring])
(require '[criterium.core :as cc])

(defn create [options]
  (ring/ring-handler
    (ring/router
      ["/ping" (constantly {:status 200, :body "ok"})])
    (ring/create-default-handler)
    options))

;; 130ns
(let [app (create nil)]
  (cc/quick-bench
    (app {:request-method :get, :uri "/ping"})))

;; 80ns
(let [app (create {:inject-router? false, :inject-match? false})]
  (cc/quick-bench
    (app {:request-method :get, :uri "/ping"})))
```

**NOTE**: Without `Router`, you can't to do [reverse routing](ring/reverse_routing.md) and without `Match` you can't write [dynamic extensions](ring/dynamic_extensions.md).

### Performance tips

Few things that have an effect on performance:

* Wildcard-routes are an order of magnitude slower than static routes
* Conflicting routes are served with LinearRouter, which is the slowest implementation.
* It's ok to mix non-wildcard, wildcard or even conflicting routes in a same routing tree. Reitit will create an hierarchy of routers to serve all the routes with best possible implementation.
* Move computation from request processing time into creation time, using by compiling [middleware](ring/compiling_middleware.md), [interceptors](http/interceptors.md) and [route data](advanced/configuring_routers.md).
  * Unmounted middleware (or interceptor) is infinitely faster than a mounted one effectively doing nothing.

```

`/Users/ovistoica/workspace/reitit/doc/README.md`:

```md
# Introduction

[Reitit](https://github.com/metosin/reitit) is a fast data-driven router for Clojure(Script).

* Simple data-driven [route syntax](./basics/route_syntax.md)
* Route [conflict resolution](./basics/route_conflicts.md)
* First-class [route data](./basics/route_data.md)
* Bi-directional routing
* [Pluggable coercion](./coercion/coercion.md) ([schema](https://github.com/plumatic/schema), [clojure.spec](https://clojure.org/about/spec), [malli](https://github.com/metosin/malli))
* Helpers for [ring](./ring/ring.md), [http](./http/interceptors.md), [pedestal](./http/pedestal.md) & [frontend](./frontend/basics.md)
* Friendly [Error Messages](./basics/error_messages.md)
* Extendable
* Modular
* [Fast](performance.md)

There is [#reitit](https://clojurians.slack.com/messages/reitit/) in [Clojurians Slack](http://clojurians.net/) for discussion & help.

## Main Modules

* `reitit` - all bundled
* `reitit-core` - the routing core
* `reitit-ring` - a [ring router](./ring/ring.md)
* `reitit-middleware` - [common middleware](./ring/default_middleware.md) for `reitit-ring`
* `reitit-spec` [clojure.spec](https://clojure.org/about/spec) coercion
* `reitit-schema` [Schema](https://github.com/plumatic/schema) coercion
* `reitit-swagger` [Swagger2](https://swagger.io/) apidocs
* `reitit-openapi` OpenAPI 3 apidocs
* `reitit-swagger-ui` Integrated [Swagger UI](https://github.com/swagger-api/swagger-ui).
* `reitit-frontend` Tools for [frontend routing](frontend/basics.md)
* `reitit-http` http-routing with Pedestal-style Interceptors
* `reitit-interceptors` - [common interceptors](./http/default_interceptors.md) for `reitit-http`
* `reitit-sieppari` support for [Sieppari](https://github.com/metosin/sieppari) Interceptors
* `reitit-dev` - development utilities

## Extra modules

* `reitit-pedestal` support for [Pedestal](http://pedestal.io)

## Latest version

All bundled:

```clj
[metosin/reitit "0.8.0"]
```

Optionally, the parts can be required separately.

# Examples

## Simple router

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    [["/api/ping" ::ping]
     ["/api/orders/:id" ::order-by-id]]))
```

Routing:

```clj
(r/match-by-path router "/api/ipa")
; nil

(r/match-by-path router "/api/ping")
; #Match{:template "/api/ping"
;        :data {:name ::ping}
;        :result nil
;        :path-params {}
;        :path "/api/ping"}

(r/match-by-path router "/api/orders/1")
; #Match{:template "/api/orders/:id"
;        :data {:name ::order-by-id}
;        :result nil
;        :path-params {:id "1"}
;        :path "/api/orders/1"}
```

Reverse-routing:

```clj
(r/match-by-name router ::ipa)
; nil

(r/match-by-name router ::ping)
; #Match{:template "/api/ping"
;        :data {:name ::ping}
;        :result nil
;        :path-params {}
;        :path "/api/ping"}

(r/match-by-name router ::order-by-id)
; #PartialMatch{:template "/api/orders/:id"
;               :data {:name :user/order-by-id}
;               :result nil
;               :path-params nil
;               :required #{:id}}

(r/partial-match? (r/match-by-name router ::order-by-id))
; true

(r/match-by-name router ::order-by-id {:id 2})
; #Match{:template "/api/orders/:id",
;        :data {:name ::order-by-id},
;        :result nil,
;        :path-params {:id 2},
;        :path "/api/orders/2"}
```

## Ring router

A Ring router function adds support for `:handler` functions, `:middleware` and routing based on `:request-method`. It also supports pluggable parameter coercion (`clojure.spec`), data-driven middleware, route and middleware compilation, dynamic extensions and more.

```clj
(require '[reitit.ring :as ring])

(defn handler [_]
  {:status 200, :body "ok"})

(defn wrap [handler id]
  (fn [request]
    (update (handler request) :wrap (fnil conj '()) id)))

(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[wrap :api]]}
       ["/ping" {:get handler
                 :name ::ping}]
       ["/admin" {:middleware [[wrap :admin]]}
        ["/users" {:get handler
                   :post handler}]]])))
```

Routing:

```clj
(app {:request-method :get, :uri "/api/admin/users"})
; {:status 200, :body "ok", :wrap (:api :admin)}

(app {:request-method :put, :uri "/api/admin/users"})
; nil
```

Reverse-routing:

```clj
(require '[reitit.core :as r])

(-> app (ring/get-router) (r/match-by-name ::ping))
; #Match{:template "/api/ping"
;        :data {:middleware [[#object[user$wrap] :api]]
;               :get {:handler #object[user$handler]}
;        :name ::ping}
;        :result #Methods{...}
;        :path-params nil
;        :path "/api/ping"}
```

```

`/Users/ovistoica/workspace/reitit/doc/http/transforming_interceptor_chain.md`:

```md
# Transforming the Interceptor Chain

There is an extra option in http-router (actually, in the underlying interceptor-router): `:reitit.interceptor/transform` to transform the interceptor chain per endpoint. Value should be a function or a vector of functions that get a vector of compiled interceptors and should return a new vector of interceptors.

**Note:** the last interceptor in the chain is usually the handler, compiled into an Interceptor. Applying a transformation `clojure.core/reverse` would put this interceptor into first in the chain, making the rest of the interceptors effectively unreachable. There is a helper `reitit.interceptor/transform-butlast` to transform all but the last interceptor.

## Example Application

```clj
(require '[reitit.http :as http])
(require '[reitit.interceptor.sieppari :as sieppari])

(defn interceptor [message]
  {:enter (fn [ctx] (update-in ctx [:request :message] (fnil conj []) message))})

(defn handler [req]
  {:status 200
   :body (select-keys req [:message])})

(def app
  (http/ring-handler
    (http/router
      ["/api" {:interceptors [(interceptor 1) (interceptor 2)]}
       ["/ping" {:get {:interceptors [(interceptor 3)]
                       :handler handler}}]])
    {:executor sieppari/executor}))

(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body {:message [1 2 3]}}

```

### Reversing the Interceptor Chain

```clj
(def app
  (http/ring-handler
    (http/router
      ["/api" {:interceptors [(interceptor 1) (interceptor 2)]}
       ["/ping" {:get {:interceptors [(interceptor 3)]
                       :handler handler}}]]
      {::interceptor/transform (interceptor/transform-butlast reverse)})
    {:executor sieppari/executor}))

(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body {:message [3 2 1]}}
```

### Interleaving Interceptors

```clj
(def app
  (http/ring-handler
    (http/router
      ["/api" {:interceptors [(interceptor 1) (interceptor 2)]}
       ["/ping" {:get {:interceptors [(interceptor 3)]
                       :handler handler}}]]
      {::interceptor/transform #(interleave % (repeat (interceptor :debug)))})
    {:executor sieppari/executor}))

(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body {:message [1 :debug 2 :debug 3 :debug]}}
```

### Printing Context Diffs

```clj
[metosin/reitit-interceptors "0.8.0"]
```

Using `reitit.http.interceptors.dev/print-context-diffs` transformation, the context diffs between each interceptor are printed out to the console. To use it, add the following router option:

```clj
:reitit.interceptor/transform reitit.http.interceptor.dev/print-context-diffs
```

Sample output:

![Http Context Diff](../images/http-context-diff.png)

Sample applications (uncomment the option to see the diffs):

* Sieppari: https://github.com/metosin/reitit/blob/master/examples/http-swagger/src/example/server.clj
* Pedestal: https://github.com/metosin/reitit/blob/master/examples/pedestal-swagger/src/example/server.clj

```

`/Users/ovistoica/workspace/reitit/doc/http/pedestal.md`:

```md
# Pedestal

[Pedestal](http://pedestal.io/) is a backend web framework for Clojure. `reitit-pedestal` provides an alternative routing engine for Pedestal.

```clj
[metosin/reitit-pedestal "0.8.0"]
```

Why should one use reitit instead of the Pedestal [default routing](http://pedestal.io/reference/routing-quick-reference)?

* One simple [route syntax](../basics/route_syntax.md), with full [route conflict resolution](../basics/route_conflicts.md).
* Supports [first class route data](../basics/route_data.md) with [spec validation](../basics/route_data_validation.md).
* Fixes some [known problems](https://github.com/pedestal/pedestal/issues/532) in routing.
* Can handle [trailing backslashes](../ring/slash_handler.md).
* One router for both backend and [frontend](../frontend/basics.md).
* Supports [parameter coercion](../ring/coercion.md) & [Swagger](../ring/swagger.md).
* Is even [faster](../performance.md).

To use Pedestal with reitit, you should first read both the [Pedestal docs](http://pedestal.io/) and the [reitit interceptor guide](interceptors.md).


## Example

A minimalistic example on how to to swap the default-router with a reitit router.

```clj
; [io.pedestal/pedestal.service "0.5.5"]
; [io.pedestal/pedestal.jetty "0.5.5"]
; [metosin/reitit-pedestal "0.8.0"]
; [metosin/reitit "0.8.0"]

(require '[io.pedestal.http :as server])
(require '[reitit.pedestal :as pedestal])
(require '[reitit.http :as http])
(require '[reitit.ring :as ring])

(defn interceptor [number]
  {:enter (fn [ctx] (update-in ctx [:request :number] (fnil + 0) number))})

(def routes
  ["/api"
   {:interceptors [(interceptor 1)]}

   ["/number"
    {:interceptors [(interceptor 10)]
     :get {:interceptors [(interceptor 100)]
           :handler (fn [req]
                      {:status 200
                       :body (select-keys req [:number])})}}]])

(-> {::server/type :jetty
     ::server/port 3000
     ::server/join? false
     ;; no pedestal routes
     ::server/routes []}
    (server/default-interceptors)
    ;; swap the reitit router
    (pedestal/replace-last-interceptor
      (pedestal/routing-interceptor
        (http/router routes)))
    (server/dev-interceptors)
    (server/create-server)
    (server/start))
```

## Compatibility

There is no common interceptor spec for Clojure and all default reitit interceptors (coercion, exceptions etc.) use the [Sieppari](https://github.com/metosin/sieppari) interceptor model. It is mostly compatible with the Pedestal Interceptor model, only exception being that the `:error` handlers take just 1 arity (`context`) compared to [Pedestal's 2-arity](http://pedestal.io/reference/error-handling) (`context` and `exception`).

Currently, out of the reitit default interceptors, there is only the `reitit.http.interceptors.exception/exception-interceptor` which has the `:error` defined.

You are most welcome to discuss about a common interceptor spec in [#interceptors](https://clojurians.slack.com/messages/interceptors/) on [Clojurians Slack](http://clojurians.net/).

## More examples

### Simple

Simple example with sync & async interceptors: https://github.com/metosin/reitit/tree/master/examples/pedestal

### Swagger

More complete example with custom interceptors, [default interceptors](default_interceptors.md), [coercion](../coercion/coercion.md) and [swagger](../ring/swagger.md)-support enabled: https://github.com/metosin/reitit/tree/master/examples/pedestal-swagger

```

`/Users/ovistoica/workspace/reitit/doc/http/interceptors.md`:

```md
# Interceptors

Reitit has also support for [interceptors](http://pedestal.io/reference/interceptors) as an alternative to using middleware. Basic interceptor handling is implemented in `reitit.interceptor` package.  There is no interceptor executor shipped, but you can use libraries like [Pedestal Interceptor](https://github.com/pedestal/pedestal/tree/master/interceptor) or [Sieppari](https://github.com/metosin/sieppari) to execute the chains.

## Reitit-http

```clj
[metosin/reitit-http "0.8.0"]
```

A module for http-routing using interceptors instead of middleware. Builds on top of the [`reitit-ring`](../ring/ring.md) module having all the same features.

The differences:

* `:interceptors` key used in route data instead of `:middleware`
* `reitit.http/http-router` requires an extra option `:executor` of type `reitit.interceptor/Executor` to execute the interceptor chain
   * optionally, a routing interceptor can be used - it enqueues the matched interceptors into the context. See `reitit.http/routing-interceptor` for details.

## Simple example

```clj
(require '[reitit.ring :as ring])
(require '[reitit.http :as http])
(require '[reitit.interceptor.sieppari :as sieppari])

(defn interceptor [number]
  {:enter (fn [ctx] (update-in ctx [:request :number] (fnil + 0) number))})

(def app
  (http/ring-handler
    (http/router
      ["/api"
       {:interceptors [(interceptor 1)]}

       ["/number"
        {:interceptors [(interceptor 10)]
         :get {:interceptors [(interceptor 100)]
               :handler (fn [req]
                          {:status 200
                           :body (select-keys req [:number])})}}]])

    ;; the default handler
    (ring/create-default-handler)

    ;; executor
    {:executor sieppari/executor}))


(app {:request-method :get, :uri "/"})
; {:status 404, :body "", :headers {}}

(app {:request-method :get, :uri "/api/number"})
; {:status 200, :body {:number 111}}
```

## Why interceptors?

* https://quanttype.net/posts/2018-08-03-why-interceptors.html
* https://www.reddit.com/r/Clojure/comments/9csmty/why_interceptors/

```

`/Users/ovistoica/workspace/reitit/doc/http/sieppari.md`:

```md
# Sieppari

```clj
[metosin/reitit-sieppari "0.8.0"]
```

[Sieppari](https://github.com/metosin/sieppari) is a new and fast interceptor implementation for Clojure, with pluggable async supporting [core.async](https://github.com/clojure/core.async), [Manifold](https://github.com/ztellman/manifold) and [Promesa](http://funcool.github.io/promesa/latest).

To use Sieppari with `reitit-http`, we need to attach a `reitit.interceptor.sieppari/executor` to a `http-router` to compile and execute the interceptor chains. Reitit and Sieppari share the same interceptor model, so all reitit default interceptors work seamlessly together.

We can use both synchronous ring and [async-ring](https://www.booleanknot.com/blog/2016/07/15/asynchronous-ring.html) with Sieppari.

## Synchronous Ring

```clj
(require '[reitit.http :as http])
(require '[reitit.interceptor.sieppari :as sieppari])

(defn i [x]
  {:enter (fn [ctx] (println "enter " x) ctx)
   :leave (fn [ctx] (println "leave " x) ctx)})

(defn handler [_]
  (future {:status 200, :body "pong"}))

(def app
  (http/ring-handler
    (http/router
      ["/api"
       {:interceptors [(i :api)]}

       ["/ping"
        {:interceptors [(i :ping)]
         :get {:interceptors [(i :get)]
               :handler handler}}]])
    {:executor sieppari/executor}))

(app {:request-method :get, :uri "/api/ping"})
;enter  :api
;enter  :ping
;enter  :get
;leave  :get
;leave  :ping
;leave  :api
;=> {:status 200, :body "pong"}
```

## Async-ring

```clj
(let [respond (promise)]
  (app {:request-method :get, :uri "/api/ping"} respond nil)
  (deref respond 1000 ::timeout))
;enter  :api
;enter  :ping
;enter  :get
;leave  :get
;leave  :ping
;leave  :api
;=> {:status 200, :body "pong"}
```

## Examples

### Simple

* simple example, with both sync & async code:
  * https://github.com/metosin/reitit/tree/master/examples/http

### With batteries

* with [default interceptors](default_interceptors.md), [coercion](../coercion/coercion.md) and [swagger](../ring/swagger.md)-support:
  * https://github.com/metosin/reitit/tree/master/examples/http-swagger

```

`/Users/ovistoica/workspace/reitit/doc/http/default_interceptors.md`:

```md
# Default Interceptors

```clj
[metosin/reitit-interceptors "0.8.0"]
```

Just like the [ring default middleware](../ring/default_middleware.md), but for interceptors.

### Parameters handling
* `reitit.http.interceptors.parameters/parameters-interceptor`

### Exception handling
* `reitit.http.interceptors.exception/exception-interceptor`

### Content Negotiation
* `reitit.http.interceptors.muuntaja/format-interceptor`
* `reitit.http.interceptors.muuntaja/format-negotiate-interceptor`
* `reitit.http.interceptors.muuntaja/format-request-interceptor`
* `reitit.http.interceptors.muuntaja/format-response-interceptor`

### Multipart request handling
* `reitit.http.interceptors.multipart/multipart-interceptor`

## Example app

See an example app with the default interceptors in action: https://github.com/metosin/reitit/blob/master/examples/http-swagger/src/example/server.clj.

```

`/Users/ovistoica/workspace/reitit/doc/ring/middleware_registry.md`:

```md
# Middleware Registry

The `:middleware` syntax in `reitit-ring` also supports Keywords. Keywords are looked up from the Middleware Registry, which is a map of `keyword => IntoMiddleware`. Middleware registry should be stored under key `:reitit.middleware/registry` in the router options. If a middleware keyword isn't found in the registry, router creation fails fast with a descriptive error message.

## Examples

Application using middleware defined in the Middleware Registry:

```clj
(require '[reitit.ring :as ring])
(require '[reitit.middleware :as middleware])

(defn wrap-bonus [handler value]
  (fn [request]
    (handler (update request :bonus (fnil + 0) value))))

(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[:bonus 20]]}
       ["/bonus" {:middleware [:bonus10]
                 :get (fn [{:keys [bonus]}]
                        {:status 200, :body {:bonus bonus}})}]]
      {::middleware/registry {:bonus wrap-bonus
                              :bonus10 [:bonus 10]}})))
```

Works as expected:

```clj
(app {:request-method :get, :uri "/api/bonus"})
; {:status 200, :body {:bonus 30}}
```

Router creation fails fast if the registry doesn't contain the middleware:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[:bonus 20]]}
       ["/bonus" {:middleware [:bonus10]
                  :get (fn [{:keys [bonus]}]
                         {:status 200, :body {:bonus bonus}})}]]
      {::middleware/registry {:bonus wrap-bonus}})))
;CompilerException clojure.lang.ExceptionInfo: Middleware :bonus10 not found in registry.
;
;Available middleware in registry:
;
;|    :id |                         :description |
;|--------+--------------------------------------|
;| :bonus | reitit.ring_test$wrap_bonus@59fddabb |
```

## When to use the registry?

Middleware as Keywords helps to keep the routes (all but handlers) as literal data (i.e. data that evaluates to itself), enabling the routes to be persisted in external formats like EDN-files and databases. Duct is a good example, where the [middleware can be referenced from EDN-files](https://github.com/duct-framework/duct/wiki/Configuration). It should be easy to make Duct configuration a Middleware Registry in `reitit-ring`.

On the other hand, it's an extra level of indirection, making things more complex and removing the default IDE support of "go to definition" or "look up source".

## TODO

* a prefilled registry of common middleware in the `reitit-middleware`

```

`/Users/ovistoica/workspace/reitit/doc/ring/default_handler.md`:

```md
# Default handler

By default, if no routes match, `nil` is returned, which is not a valid response in Ring:

```clj
(require '[reitit.ring :as ring])

(defn handler [_]
  {:status 200, :body ""})

(def app
  (ring/ring-handler
    (ring/router
      ["/ping" handler])))

(app {:uri "/invalid"})
; nil
```

Setting the default-handler as a second argument to `ring-handler`:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/ping" handler])
    (constantly {:status 404, :body ""})))

(app {:uri "/invalid"})
; {:status 404, :body ""}
```

To get more correct http error responses, `ring/create-default-handler` can be used. It differentiates `:not-found` (no route matched), `:method-not-allowed` (no method matched) and `:not-acceptable` (handler returned `nil`).

With defaults:

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/ping" {:get handler}]
       ["/pong" (constantly nil)]])
    (ring/create-default-handler)))

(app {:request-method :get, :uri "/ping"})
; {:status 200, :body ""}

(app {:request-method :get, :uri "/"})
; {:status 404, :body ""}

(app {:request-method :post, :uri "/ping"})
; {:status 405, :body ""}

(app {:request-method :get, :uri "/pong"})
; {:status 406, :body ""}
```

With custom responses:

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/ping" {:get handler}]
       ["/pong" (constantly nil)]])
    (ring/create-default-handler
      {:not-found (constantly {:status 404, :body "kosh"})
       :method-not-allowed (constantly {:status 405, :body "kosh"})
       :not-acceptable (constantly {:status 406, :body "kosh"})})))

(app {:request-method :get, :uri "/ping"})
; {:status 200, :body ""}

(app {:request-method :get, :uri "/"})
; {:status 404, :body "kosh"}

(app {:request-method :post, :uri "/ping"})
; {:status 405, :body "kosh"}

(app {:request-method :get, :uri "/pong"})
; {:status 406, :body "kosh"}
```

```

`/Users/ovistoica/workspace/reitit/doc/ring/exceptions.md`:

```md
# Exception Handling with Ring

```clj
[metosin/reitit-middleware "0.8.0"]
```

Exceptions thrown in router creation can be [handled with custom exception handler](../basics/error_messages.md). By default, exceptions thrown at runtime from a handler or a middleware are not caught by the `reitit.ring/ring-handler`. A good practice is to have a top-level exception handler to log and format errors for clients.

```clj
(require '[reitit.ring.middleware.exception :as exception])
```

### `exception/exception-middleware`

A preconfigured middleware using `exception/default-handlers`. Catches:

* Request & response [Coercion](coercion.md) exceptions
* [Muuntaja](https://github.com/metosin/muuntaja) decode exceptions
* Exceptions with `:type` of `:reitit.ring/response`, returning `:response` key from `ex-data`.
* Safely all other exceptions

```clj
(require '[reitit.ring :as ring])

(def app
  (ring/ring-handler
    (ring/router
      ["/fail" (fn [_] (throw (Exception. "fail")))]
      {:data {:middleware [exception/exception-middleware]}})))

(app {:request-method :get, :uri "/fail"})
;{:status 500
; :body {:type "exception"
;        :class "java.lang.Exception"}}
```

### `exception/create-exception-middleware`

Creates the exception-middleware with custom options. Takes a map of `identifier => exception request => response` that is used to select the exception handler for the thrown/raised exception identifier. Exception identifier is either a `Keyword` or an Exception Class.

The following handlers are available by default:

| key                                  | description
|--------------------------------------|-------------
| `:reitit.ring/response`              | value in ex-data key `:response` will be returned
| `:muuntaja/decode`                   | handle Muuntaja decoding exceptions
| `:reitit.coercion/request-coercion`  | request coercion errors (http 400 response)
| `:reitit.coercion/response-coercion` | response coercion errors (http 500 response)
| `::exception/default`                | a default exception handler if nothing else matched (default `exception/default-handler`).
| `::exception/wrap`                   | a 3-arity handler to wrap the actual handler `handler exception request => response` (no default).

The handler is selected from the options map by exception identifier in the following lookup order:

1) `:type` of exception ex-data
2) Class of exception
3) `:type` ancestors of exception ex-data
4) Super Classes of exception
5) The `::default` handler

```clj
;; type hierarchy
(derive ::error ::exception)
(derive ::failure ::exception)
(derive ::horror ::exception)

(defn handler [message exception request]
  {:status 500
   :body {:message message
          :exception (.getClass exception)
          :data (ex-data exception)
          :uri (:uri request)}})

(def exception-middleware
  (exception/create-exception-middleware
    (merge
      exception/default-handlers
      {;; ex-data with :type ::error
       ::error (partial handler "error")

       ;; ex-data with ::exception or ::failure
       ::exception (partial handler "exception")

       ;; SQLException and all it's child classes
       java.sql.SQLException (partial handler "sql-exception")

       ;; override the default handler
       ::exception/default (partial handler "default")

       ;; print stack-traces for all exceptions
       ::exception/wrap (fn [handler e request]
                          (println "ERROR" (pr-str (:uri request)))
                          (handler e request))})))

(def app
  (ring/ring-handler
    (ring/router
      ["/fail" (fn [_] (throw (ex-info "fail" {:type ::failure})))]
      {:data {:middleware [exception-middleware]}})))

(app {:request-method :get, :uri "/fail"})
; ERROR "/fail"
; => {:status 500,
;     :body {:message "default"
;            :exception clojure.lang.ExceptionInfo
;            :data {:type :user/failure}
;            :uri "/fail"}}
```

```

`/Users/ovistoica/workspace/reitit/doc/ring/static.md`:

```md
# Static Resources (Clojure Only)

Static resources can be served by using the following two functions:

* `reitit.ring/create-resource-handler`, which returns a Ring handler that serves files from classpath, and
* `reitit.ring/create-file-handler`, which returns a Ring handler that servers files from file system

There are two ways to mount the handlers.
The examples below use `reitit.ring/create-resource-handler`, but `reitit.ring/create-file-handler` works the same way.

## Internal routes

This is good option if static files can be from non-conflicting paths, e.g. `"/assets/*"`.

```clj
(require '[reitit.ring :as ring])

(ring/ring-handler
  (ring/router
    [["/ping" (constantly {:status 200, :body "pong"})]
     ["/assets/*" (ring/create-resource-handler)]])
  (ring/create-default-handler))
```

To serve static files with conflicting routes, e.g. `"/*"`, one needs to disable the conflict resolution:

```clj
(require '[reitit.ring :as ring])

(ring/ring-handler
  (ring/router
    [["/ping" (constantly {:status 200, :body "pong"})]
     ["/*" (ring/create-resource-handler)]]
    {:conflicts (constantly nil)})
  (ring/create-default-handler))
```

## External routes

A better way to serve files from conflicting paths, e.g. `"/*"`, is to serve them from the default-handler.
One can compose multiple default locations using `reitit.ring/ring-handler`.
This way, they are only served if none of the actual routes have matched.

```clj
(ring/ring-handler
  (ring/router
    ["/ping" (constantly {:status 200, :body "pong"})])
  (ring/routes
    (ring/create-resource-handler {:path "/"})
    (ring/create-default-handler)))
```

## Configuration

`reitit.ring/create-file-handler` and `reitit.ring/create-resource-handler` take optionally an options map to configure how the files are being served.

| key                 | description |
| --------------------|-------------|
| :parameter          | optional name of the wildcard parameter, defaults to unnamed keyword `:`
| :root               | optional resource root, defaults to `\"public\"`
| :path               | path to mount the handler to. Required when mounted outside of a router, does not work inside a router.
| :loader             | optional class loader to resolve the resources
| :index-files        | optional vector of index-files to look in a resource directory, defaults to `[\"index.html\"]`
| :index-redirect?    | optional boolean: if true (default false), redirect to index file, if false serve it directly
| :canonicalize-uris? | optional boolean: if true (default), try to serve index files for non directory paths (paths that end with slash)
| :not-found-handler  | optional handler function to use if the requested resource is missing (404 Not Found)


### TODO

* support for things like `:cache`, `:etag`, `:last-modified?`, and `:gzip`
* support for ClojureScript

```

`/Users/ovistoica/workspace/reitit/doc/ring/coercion.md`:

```md
# Ring Coercion

Basic coercion is explained in detail [in the Coercion Guide](../coercion/coercion.md). With Ring, both request parameters and response bodies can be coerced.

The following request parameters are currently supported:

| type         | request source                                   |
|--------------|--------------------------------------------------|
| `:query`     | `:query-params`                                  |
| `:body`      | `:body-params`                                   |
| `:request`   | `:body-params`, allows per-content-type coercion |
| `:form`      | `:form-params`                                   |
| `:header`    | `:header-params`                                 |
| `:path`      | `:path-params`                                   |
| `:multipart` | `:multipart-params`, see [Default Middleware](default_middleware.md) |

To enable coercion, the following things need to be done:

* Define a `reitit.coercion/Coercion` for the routes
* Define types for the parameters and/or responses
* Mount Coercion Middleware to apply to coercion
* Use the coerced parameters in a handler/middleware

## Define coercion

`reitit.coercion/Coercion` is a protocol defining how types are defined, coerced and inventoried.

Reitit ships with the following coercion modules:

* `reitit.coercion.malli/coercion` for [malli](https://github.com/metosin/malli)
* `reitit.coercion.schema/coercion` for [plumatic schema](https://github.com/plumatic/schema)
* `reitit.coercion.spec/coercion` for both [clojure.spec](https://clojure.org/about/spec) and [data-specs](https://github.com/metosin/spec-tools#data-specs)

Coercion can be attached to route data under `:coercion` key. There can be multiple `Coercion` implementations within a single router, normal [scoping rules](../basics/route_data.md#nested-route-data) apply.

## Defining parameters and responses

Parameters are defined in route data under `:parameters` key. It's value should be a map of parameter `:type` -> Coercion Schema.

Responses are defined in route data under `:responses` key. It's value should be a map of http status code to a map which can contain `:body` key with Coercion Schema as value.

Below is an example with [Plumatic Schema](https://github.com/plumatic/schema). It defines schemas for `:query`, `:body` and `:path` parameters and for http 200 response `:body`.

Handlers can access the coerced parameters via the `:parameters` key in the request.

```clj
(require '[reitit.coercion.schema])
(require '[schema.core :as s])

(def PositiveInt (s/constrained s/Int pos? 'PositiveInt))

(def plus-endpoint
  {:coercion reitit.coercion.schema/coercion
   :parameters {:query {:x s/Int}
                :body {:y s/Int}
                :path {:z s/Int}}
   :responses {200 {:body {:total PositiveInt}}}
   :handler (fn [{:keys [parameters]}]
              (let [total (+ (-> parameters :query :x)
                             (-> parameters :body :y)
                             (-> parameters :path :z))]
                {:status 200
                 :body {:total total}}))})
```


### Nested parameter definitions

Parameters are accumulated recursively along the route tree, just like
other [route data](../basics/route_data.md). There is special case
handling for merging eg. malli `:map` schemas.

```clj
(def router
 (reitit.ring/router
   ["/api" {:get {:parameters {:query [:map [:api-key :string]]}}}
    ["/project/:project-id" {:get {:parameters {:path [:map [:project-id :int]]}}}
     ["/task/:task-id" {:get {:parameters {:path [:map [:task-id :int]]
                                           :query [:map [:details :boolean]]}
                              :handler (fn [req] (prn req))}}]]]
   {:data {:coercion reitit.coercion.malli/coercion}}))
```

```clj
(-> (r/match-by-path router "/api/project/1/task/2") :result :get :data :parameters)
; {:query [:map
;          {:closed true}
;          [:api-key :string]
;          [:details :boolean]],
;  :path [:map
;         {:closed true}
;         [:project-id :int]
;         [:task-id :int]]}
```

## Coercion Middleware

Defining a coercion for a route data doesn't do anything, as it's just data. We have to attach some code to apply the actual coercion. We can use the middleware from `reitit.ring.coercion`:

* `coerce-request-middleware` to apply the parameter coercion
* `coerce-response-middleware` to apply the response coercion
* `coerce-exceptions-middleware` to transform coercion exceptions into pretty responses

### Full example

Here is a full example for applying coercion with Reitit, Ring and Schema:

```clj
(require '[reitit.ring.coercion :as rrc])
(require '[reitit.coercion.schema])
(require '[reitit.ring :as ring])
(require '[schema.core :as s])

(def PositiveInt (s/constrained s/Int pos? 'PositiveInt))

(def app
  (ring/ring-handler
    (ring/router
      ["/api"
       ["/ping" {:name ::ping
                 :get (fn [_]
                        {:status 200
                         :body "pong"})}]
       ["/plus/:z" {:name ::plus
                    :post {:coercion reitit.coercion.schema/coercion
                           :parameters {:query {:x s/Int}
                                        :body {:y s/Int}
                                        :path {:z s/Int}}
                           :responses {200 {:body {:total PositiveInt}}}
                           :handler (fn [{:keys [parameters]}]
                                      (let [total (+ (-> parameters :query :x)
                                                     (-> parameters :body :y)
                                                     (-> parameters :path :z))]
                                        {:status 200
                                         :body {:total total}}))}}]]
      {:data {:middleware [rrc/coerce-exceptions-middleware
                           rrc/coerce-request-middleware
                           rrc/coerce-response-middleware]}})))
```

Valid request:

```clj
(app {:request-method :post
      :uri "/api/plus/3"
      :query-params {"x" "1"}
      :body-params {:y 2}})
; {:status 200, :body {:total 6}}
```

Invalid request:

```clj
(app {:request-method :post
      :uri "/api/plus/3"
      :query-params {"x" "abba"}
      :body-params {:y 2}})
; {:status 400,
;  :body {:schema {:x "Int", "Any" "Any"},
;         :errors {:x "(not (integer? \"abba\"))"},
;         :type :reitit.coercion/request-coercion,
;         :coercion :schema,
;         :value {:x "abba"},
;         :in [:request :query-params]}}
```

Invalid response:

```clj
(app {:request-method :post
      :uri "/api/plus/3"
      :query-params {"x" "1"}
      :body-params {:y -10}})
; {:status 500,
;  :body {:schema {:total "(constrained Int PositiveInt)"},
;         :errors {:total "(not (PositiveInt -6))"},
;         :type :reitit.coercion/response-coercion,
;         :coercion :schema,
;         :value {:total -6},
;         :in [:response :body]}}
```

## Per-content-type coercion

You can also specify request and response body schemas per
content-type. These are also read by the [OpenAPI
feature](./openapi.md) when generating api docs. The syntax for this
is:

```clj
(def app
  (ring/ring-handler
   (ring/router
    ["/api"
     ["/example" {:post {:coercion reitit.coercion.schema/coercion
                         :request {:content {"application/json" {:schema {:y s/Int}}
                                             "application/edn" {:schema {:z s/Int}}
                                             ;; default if no content-type matches:
                                             :default {:schema {:yy s/Int}}}}
                         :responses {200 {:content {"application/json" {:schema {:w s/Int}}
                                                    "application/edn" {:schema {:x s/Int}}
                                                    :default {:schema {:ww s/Int}}}}}
                         :handler ...}}]]
    {:data {:middleware [rrc/coerce-exceptions-middleware
                         rrc/coerce-request-middleware
                         rrc/coerce-response-middleware]}})))
```

## Pretty printing spec errors

Spec problems are exposed as is in request & response coercion errors. Pretty-printers like [expound](https://github.com/bhb/expound) can be enabled like this:

```clj
(require '[reitit.ring :as ring])
(require '[reitit.ring.middleware.exception :as exception])
(require '[reitit.ring.coercion :as coercion])
(require '[expound.alpha :as expound])

(defn coercion-error-handler [status]
  (let [printer (expound/custom-printer {:theme :figwheel-theme, :print-specs? false})
        handler (exception/create-coercion-handler status)]
    (fn [exception request]
      (printer (-> exception ex-data :problems))
      (handler exception request))))

(def app
  (ring/ring-handler
    (ring/router
      ["/plus"
       {:get
        {:parameters {:query {:x int?, :y int?}}
         :responses {200 {:body {:total pos-int?}}}
         :handler (fn [{{{:keys [x y]} :query} :parameters}]
                    {:status 200, :body {:total (+ x y)}})}}]
      {:data {:coercion reitit.coercion.spec/coercion
              :middleware [(exception/create-exception-middleware
                             (merge
                               exception/default-handlers
                               {:reitit.coercion/request-coercion (coercion-error-handler 400)
                                :reitit.coercion/response-coercion (coercion-error-handler 500)}))
                           coercion/coerce-request-middleware
                           coercion/coerce-response-middleware]}})))

(app
  {:uri "/plus"
   :request-method :get
   :query-params {"x" "1", "y" "fail"}})
; => ...
; -- Spec failed --------------------
;
;   {:x ..., :y "fail"}
;                ^^^^^^
;
; should satisfy
;
;   int?



(app
  {:uri "/plus"
   :request-method :get
   :query-params {"x" "1", "y" "-2"}})
; => ...
;-- Spec failed --------------------
;
;   {:total -1}
;           ^^
;
; should satisfy
;
;   pos-int?
```

### Optimizations

The coercion middlewares are [compiled against a route](compiling_middleware.md). In the middleware compilation step the actual coercer implementations are constructed for the defined models. Also, the middleware doesn't mount itself if a route doesn't have `:coercion` and `:parameters` or `:responses` defined.

We can query the compiled middleware chain for the routes:

```clj
(require '[reitit.core :as r])

(-> (ring/get-router app)
    (r/match-by-name ::plus)
    :result :post :middleware
    (->> (mapv :name)))
; [::mw/coerce-exceptions
;  ::mw/coerce-request
;  ::mw/coerce-response]
```

Route without coercion defined:

```clj
(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body "pong"}
```

Has no mounted middleware:

```clj
(-> (ring/get-router app)
    (r/match-by-name ::ping)
    :result :get :middleware
    (->> (mapv :name)))
; []
```

```

`/Users/ovistoica/workspace/reitit/doc/ring/default_middleware.md`:

```md
# Default Middleware

```clj
[metosin/reitit-middleware "0.8.0"]
```

Any Ring middleware can be used with `reitit-ring`, but using data-driven middleware is preferred as they are easier to manage and in many cases yield better performance. `reitit-middleware` contains a set of common ring middleware, lifted into data-driven middleware.

* [Parameter Handling](#parameters-handling)
* [Exception Handling](#exception-handling)
* [Content Negotiation](#content-negotiation)
* [Multipart Request Handling](#multipart-request-handling)
* [Inspecting Middleware Chain](#inspecting-middleware-chain)

## Parameters Handling

`reitit.ring.middleware.parameters/parameters-middleware` to capture query- and form-params. Wraps
`ring.middleware.params/wrap-params`.

**NOTE**: This middleware will be factored into two parts: a query-parameters middleware and a Muuntaja format responsible for the the `application/x-www-form-urlencoded` body format.  cf. https://github.com/metosin/reitit/issues/134

## Exception Handling

See [Exception Handling with Ring](exceptions.md).

## Content Negotiation

See [Content Negotiation](content_negotiation.md).

## Multipart Request Handling

Wrapper for [Ring Multipart Middleware](https://github.com/ring-clojure/ring/blob/master/ring-core/src/ring/middleware/multipart_params.clj). Emits swagger `:consumes` definitions automatically.

Expected route data:

| key          | description |
| -------------|-------------|
| `[:parameters :multipart]`  | mounts only if defined for a route.


```clj
(require '[reitit.ring.middleware.multipart :as multipart])
```

* `multipart/multipart-middleware` a preconfigured middleware for multipart handling
* `multipart/create-multipart-middleware` to generate with custom configuration

## Inspecting Middleware Chain

`reitit.ring.middleware.dev/print-request-diffs` is a [middleware chain transforming function](transforming_middleware_chain.md). It prints a request and response diff between each middleware. To use it, add the following router option:

```clj
:reitit.middleware/transform reitit.ring.middleware.dev/print-request-diffs
```

Partial sample output:

![Opensensors perf test](../images/ring-request-diff.png)

## Example app

See an example app with the default middleware in action: <https://github.com/metosin/reitit/blob/master/examples/ring-malli-swagger/src/example/server.clj>.

```

`/Users/ovistoica/workspace/reitit/doc/ring/ring.md`:

```md
# Ring Router

[Ring](https://github.com/ring-clojure/ring) is a Clojure web applications library inspired by Python's WSGI and Ruby's Rack. By abstracting the details of HTTP into a simple, unified API, Ring allows web applications to be constructed of modular components that can be shared among a variety of applications, web servers, and web frameworks.

Read more about the [Ring Concepts](https://github.com/ring-clojure/ring/wiki/Concepts).

```clj
[metosin/reitit-ring "0.8.0"]
```

## `reitit.ring/router`

`reitit.ring/router` is a higher order router, which adds support for `:request-method` based routing, [handlers](https://github.com/ring-clojure/ring/wiki/Concepts#handlers) and [middleware](https://github.com/ring-clojure/ring/wiki/Concepts#middleware).

It accepts the following options:

| key                                     | description |
| ----------------------------------------|-------------|
| `:reitit.middleware/transform`          | Function of `[Middleware] => [Middleware]` to transform the expanded Middleware (default: identity).
| `:reitit.middleware/registry`           | Map of `keyword => IntoMiddleware` to replace keyword references into Middleware
| `:reitit.ring/default-options-endpoint` | Default endpoint for `:options` method (default: default-options-endpoint)

Example router:

```clj
(require '[reitit.ring :as ring])

(defn handler [_]
  {:status 200, :body "ok"})

(def router
  (ring/router
    ["/ping" {:get handler}]))
```

Match contains `:result` compiled by `reitit.ring/router`:

```clj
(require '[reitit.core :as r])

(r/match-by-path router "/ping")
;#Match{:template "/ping"
;       :data {:get {:handler #object[...]}}
;       :result #Methods{:get #Endpoint{...}
;                        :options #Endpoint{...}}
;       :path-params {}
;       :path "/ping"}
```

## `reitit.ring/ring-handler`

Given a router from `reitit.ring/router`, optional default-handler & options, `ring-handler` function will return a valid ring handler supporting both synchronous and [asynchronous](https://www.booleanknot.com/blog/2016/07/15/asynchronous-ring.html) request handling. The following options are available:

| key               | description |
| ------------------|-------------|
| `:middleware`     | Optional sequence of middlewares that wrap the ring-handler
| `:inject-match?`  | Boolean to inject `match` into request under `:reitit.core/match` key (default true)
| `:inject-router?` | Boolean to inject `router` into request under `:reitit.core/router` key (default true)

Simple Ring app:

```clj
(def app (ring/ring-handler router))
```

Applying the handler:

```clj
(app {:request-method :get, :uri "/favicon.ico"})
; nil
```

```clj
(app {:request-method :get, :uri "/ping"})
; {:status 200, :body "ok"}
```

The router can be accessed via `get-router`:

```clj
(-> app (ring/get-router) (r/compiled-routes))
;[["/ping"
;  {:handler #object[...]}
;  #Methods{:get #Endpoint{:data {:handler #object[...]}
;                          :handler #object[...]
;                          :middleware []}
;           :options #Endpoint{:data {:handler #object[...]}
;                              :handler #object[...]
;                              :middleware []}}]]
```

# Request-method based routing

Handlers can be placed either to the top-level (all methods) or under a specific method (`:get`, `:head`, `:patch`, `:delete`, `:options`, `:post`, `:put` or `:trace`). Top-level handler is used if request-method based handler is not found.

By default, the `:options` route is generated for all paths - to enable thing like [CORS](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing).

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/all" handler]
       ["/ping" {:name ::ping
                 :get handler
                 :post handler}]])))
```

Top-level handler catches all methods:

```clj
(app {:request-method :delete, :uri "/all"})
; {:status 200, :body "ok"}
```

Method-level handler catches only the method:

```clj
(app {:request-method :get, :uri "/ping"})
; {:status 200, :body "ok"}

(app {:request-method :put, :uri "/ping"})
; nil
```

By default, `:options` is also supported (see router options to change this):

```clj
(app {:request-method :options, :uri "/ping"})
; {:status 200, :body ""}
```

Name-based reverse routing:

```clj
(-> app
    (ring/get-router)
    (r/match-by-name ::ping)
    (r/match->path))
; "/ping"
```

# Middleware

Middleware can be mounted using a `:middleware` key - either to top-level or under request method submap. Its value should be a vector of `reitit.middleware/IntoMiddleware` values. These include:

1. normal ring middleware function `handler -> request -> response`
2. vector of middleware function `[handler args*] -> request -> response` and it's arguments
3. a [data-driven middleware](data_driven_middleware.md) record or a map
4. a Keyword name, to lookup the middleware from a [Middleware Registry](middleware_registry.md)

A middleware and a handler:

```clj
(defn wrap [handler id]
  (fn [request]
    (handler (update request ::acc (fnil conj []) id))))

(defn handler [{::keys [acc]}]
  {:status 200, :body (conj acc :handler)})
```

App with nested middleware:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ;; a middleware function
      ["/api" {:middleware [#(wrap % :api)]}
       ["/ping" handler]
       ;; a middleware vector at top level
       ["/admin" {:middleware [[wrap :admin]]}
        ["/db" {:middleware [[wrap :db]]
                ;; a middleware vector at under a method
                :delete {:middleware [[wrap :delete]]
                         :handler handler}}]]])))
```

Middleware is applied correctly:

```clj
(app {:request-method :delete, :uri "/api/ping"})
; {:status 200, :body [:api :handler]}
```

```clj
(app {:request-method :delete, :uri "/api/admin/db"})
; {:status 200, :body [:api :admin :db :delete :handler]}
```

Top-level middleware, applied before any routing is done:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[mw :api]]}
       ["/get" {:get handler}]])
    nil
    {:middleware [[mw :top]]}))

(app {:request-method :get, :uri "/api/get"})
; {:status 200, :body [:top :api :ok]}
```

```

`/Users/ovistoica/workspace/reitit/doc/ring/RESTful_form_methods.md`:

```md
# RESTful form methods

When designing RESTful applications you will be doing a lot of "PATCH" and "DELETE" request, but  most browsers don't support methods other than "GET" and "POST" when it comes to submitting forms.

There is a pattern to solve this (pioneered by Rails) using a hidden "_method" field in the form and swapping out the "POST" method for whatever is in that field.

We can do this with middleware in reitit like this:
```clj
(defn- hidden-method
  [request]
  (some-> (or (get-in request [:form-params "_method"])         ;; look for "_method" field in :form-params
              (get-in request [:multipart-params "_method"]))   ;; or in :multipart-params
          clojure.string/lower-case
          keyword))

(def wrap-hidden-method
  {:name ::wrap-hidden-method
   :wrap (fn [handler]
           (fn [request]
             (if-let [fm (and (= :post (:request-method request)) ;; if this is a :post request
                              (hidden-method request))]           ;; and there is a "_method" field
               (handler (assoc request :request-method fm)) ;; replace :request-method
               (handler request))))})
```

And apply the middleware like this:
```clj
(reitit.ring/ring-handler
  (reitit.ring/router ...)
  (reitit.ring/create-default-handler)
  {:middleware
    [reitit.ring.middleware.parameters/parameters-middleware ;; needed to have :form-params in the request map
     reitit.ring.middleware.multipart/multipart-middleware   ;; needed to have :multipart-params in the request map
     wrap-hidden-method]}) ;; our hidden method wrapper
```
(NOTE: This middleware must be placed here and not inside the route data given to `reitit.ring/handler`.
This is so that our middleware is applied before reitit matches the request with a specific handler using the wrong method.)

```

`/Users/ovistoica/workspace/reitit/doc/ring/dynamic_extensions.md`:

```md
# Dynamic Extensions

`ring-handler` injects the `Match` into a request and it can be extracted at runtime with `reitit.ring/get-match`. This can be used to build ad hoc extensions to the system.

This example shows a middleware to guard routes based on user roles:

```clj
(require '[reitit.ring :as ring])
(require '[clojure.set :as set])

(defn wrap-enforce-roles [handler]
  (fn [{:keys [my-roles] :as request}]
    (let [required (some-> request (ring/get-match) :data ::roles)]
      (if (and (seq required) (not (set/subset? required my-roles)))
        {:status 403, :body "forbidden"}
        (handler request)))))
```

Mounted to an app via router data (affecting all routes):

```clj
(def handler (constantly {:status 200, :body "ok"}))

(def app
  (ring/ring-handler
    (ring/router
      [["/api"
        ["/ping" handler]
        ["/admin" {::roles #{:admin}}
         ["/ping" handler]]]]
      {:data {:middleware [wrap-enforce-roles]}})))
```

Anonymous access to public route:

```clj
(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body "ok"}
```

Anonymous access to guarded route:

```clj
(app {:request-method :get, :uri "/api/admin/ping"})
; {:status 403, :body "forbidden"}
```

Authorized access to guarded route:

```clj
(app {:request-method :get, :uri "/api/admin/ping", :my-roles #{:admin}})
; {:status 200, :body "ok"}
```

Dynamic extensions are nice, but we can do much better. See [data-driven middleware](data_driven_middleware.md) and [compiling routes](compiling_middleware.md).

```

`/Users/ovistoica/workspace/reitit/doc/ring/content_negotiation.md`:

```md
# Content Negotiation

Wrapper for [Muuntaja](https://github.com/metosin/muuntaja) middleware for content negotiation, request decoding and response encoding. Takes explicit configuration via `:muuntaja` key in route data. Emits [swagger](swagger.md) `:produces` and `:consumes` definitions automatically based on the Muuntaja configuration.

Negotiates a request body based on `Content-Type` header and response body based on `Accept` and `Accept-Charset` headers. Publishes the negotiation results as `:muuntaja/request` and `:muuntaja/response` keys into the request.

Decodes the request body into `:body-params` using the `:muuntaja/request` key in request if the `:body-params` doesn't already exist.

Encodes the response body using the `:muuntaja/response` key in request if the response doesn't have `Content-Type` header already set.

Expected route data:

| key          | description |
| -------------|-------------|
| `:muuntaja`  | `muuntaja.core/Muuntaja` instance, does not mount if not set.

```clj
(require '[reitit.ring.middleware.muuntaja :as muuntaja])
```

* `muuntaja/format-middleware` - Negotiation, request decoding and response encoding in a single Middleware
* `muuntaja/format-negotiate-middleware` - Negotiation
* `muuntaja/format-request-middleware` - Request decoding
* `muuntaja/format-response-middleware` - Response encoding

```clj
(require '[reitit.ring :as ring])
(require '[reitit.ring.coercion :as rrc])
(require '[reitit.coercion.spec :as rcs])
(require '[ring.adapter.jetty :as jetty])
(require '[muuntaja.core :as m])

(def app
  (ring/ring-handler
    (ring/router
      [["/math"
        {:post {:summary "negotiated request & response (json, edn, transit)"
                :parameters {:body {:x int?, :y int?}}
                :responses {200 {:body {:total int?}}}
                :handler (fn [{{{:keys [x y]} :body} :parameters}]
                           {:status 200
                            :body {:total (+ x y)}})}}]
       ["/xml"
        {:get {:summary "forced xml response"
               :handler (fn [_]
                          {:status 200
                           :headers {"Content-Type" "text/xml"}
                           :body "<kikka>kukka</kikka>"})}}]]
      {:data {:muuntaja m/instance
              :coercion rcs/coercion
              :middleware [muuntaja/format-middleware
                           rrc/coerce-exceptions-middleware
                           rrc/coerce-request-middleware
                           rrc/coerce-response-middleware]}})))

(jetty/run-jetty #'app {:port 3000, :join? false})
```

Testing with [httpie](https://httpie.org/):

```bash
> http POST :3000/math x:=1 y:=2

HTTP/1.1 200 OK
Content-Length: 11
Content-Type: application/json; charset=utf-8
Date: Wed, 22 Aug 2018 16:59:54 GMT
Server: Jetty(9.2.21.v20170120)

{
 "total": 3
}
```

```bash
> http :3000/xml

HTTP/1.1 200 OK
Content-Length: 20
Content-Type: text/xml
Date: Wed, 22 Aug 2018 16:59:58 GMT
Server: Jetty(9.2.21.v20170120)

<kikka>kukka</kikka>
```

You can also specify request and response schemas per content-type. See [Coercion](coercion.md) and [OpenAPI Support](openapi.md).


## Changing default parameters

The current JSON formatter used by `reitit` already has the option to parse keys as `keyword` which is a sane default in Clojure. However, if you would like to parse all the `double` as `bigdecimal` you'd need to change an option of the [JSON formatter](https://github.com/metosin/jsonista)


```clj
(def new-muuntaja-instance
  (m/create
   (assoc-in
    m/default-options
    [:formats "application/json" :decoder-opts :bigdecimals]
    true)))

```

Now you should change the `m/instance` installed in the router with the `new-muuntaja-instance`.

Here you can find more options for [JSON](https://cljdoc.org/d/metosin/jsonista/0.2.5/api/jsonista.core#object-mapper) and EDN.


## Adding custom encoder

The example below is from `muuntaja` explaining how to add a custom encoder to parse a `java.util.Date` instance.

```clj

(def muuntaja-instance
  (m/create
    (assoc-in
      m/default-options
      [:formats "application/json" :encoder-opts]
      {:date-format "yyyy-MM-dd"})))

(->> {:value (java.util.Date.)}
     (m/encode m "application/json")
     slurp)
; => "{\"value\":\"2019-10-15\"}"

```

## Putting it all together

If you inspect `m/default-options` you'll find it's only a map. This means you can compose your new muuntaja instance with as many options as you need.

```clj
(def new-muuntaja
  (m/create
   (-> m/default-options
       (assoc-in [:formats "application/json" :decoder-opts :bigdecimals] true)
       (assoc-in [:formats "application/json" :encoder-opts :date-format] "yyyy-MM-dd"))))
```

```

`/Users/ovistoica/workspace/reitit/doc/ring/swagger.md`:

```md
# Swagger Support

```
[metosin/reitit-swagger "0.8.0"]
```

Reitit supports [Swagger2](https://swagger.io/) documentation, thanks to [schema-tools](https://github.com/metosin/schema-tools) and [spec-tools](https://github.com/metosin/spec-tools). Documentation is extracted from route definitions, coercion `:parameters` and `:responses` and from a set of new documentation keys.

See also: [OpenAPI support](openapi.md).

To enable swagger-documentation for a Ring router:

1. annotate your routes with swagger-data
2. mount a swagger-handler to serve the swagger-spec
3. optionally mount a swagger-ui to visualize the swagger-spec

## Swagger data

The following route data keys contribute to the generated swagger specification:

| key           | description |
| --------------|-------------|
| :swagger      | map of any swagger-data. Can have `:id` (keyword or sequence of keywords) to identify the api
| :no-doc       | optional boolean to exclude endpoint from api docs
| :tags         | optional set of string or keyword tags for an endpoint api docs
| :summary      | optional short string summary of an endpoint
| :description  | optional long description of an endpoint. Supports http://spec.commonmark.org/
| :operationId  | optional string specifying the unique ID of an Operation

Coercion keys also contribute to the docs:

| key           | description |
| --------------|-------------|
| :parameters   | optional input parameters for a route, in a format defined by the coercion
| :responses    | optional descriptions of responses, in a format defined by coercion

There is a `reitit.swagger.swagger-feature`, which acts as both a `Middleware` and an `Interceptor` that is not participating in any request processing - it just defines the route data specs for the routes it's mounted to. It is only needed if the [route data validation](route_data_validation.md) is turned on.

## Swagger spec

To serve the actual [Swagger Specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md), there is `reitit.swagger/create-swagger-handler`. It takes no arguments and returns a ring-handler which collects at request-time data from all routes for the same swagger api and returns a formatted Swagger specification as Clojure data, to be encoded by a response formatter.

If you need to post-process the generated spec, just wrap the handler with a custom `Middleware` or an `Interceptor`.

## Swagger-ui

[Swagger-ui](https://github.com/swagger-api/swagger-ui) is a user interface to visualize and interact with the Swagger specification. To make things easy, there is a pre-integrated version of the swagger-ui as a separate module.

```
[metosin/reitit-swagger-ui "0.8.0"]
```

`reitit.swagger-ui/create-swagger-ui-handler` can be used to create a ring-handler to serve the swagger-ui. It accepts the following options:

| key              | description |
| -----------------|-------------|
| :parameter       | optional name of the wildcard parameter, defaults to unnamed keyword `:`
| :root            | optional resource root, defaults to `"swagger-ui"`
| :url             | path to swagger endpoint, defaults to `/swagger.json`
| :path            | optional path to mount the handler to. Works only if mounted outside of a router.
| :config          | parameters passed to swagger-ui as-is. See [the docs](https://github.com/swagger-api/swagger-ui/tree/2.x#parameters)

We use swagger-ui from [ring-swagger-ui](https://github.com/metosin/ring-swagger-ui), which can be easily configured from routing application. It stores files `swagger-ui` in the resource classpath.

Webjars also hosts a [version](https://github.com/webjars/swagger-ui) of the swagger-ui.

**NOTE**: Currently, swagger-ui module is just for Clojure. ClojureScript-support welcome as a PR!

**NOTE:** If you want to use swagger-ui 2.x you can do so by explicitly downgrading `metosin/ring-swagger-ui` to `2.2.10`.

**NOTE:** If you use swagger-ui 3.x, you need to include `:responses` for Swagger-UI
to display the response when trying out endpoints. You can define `:responses {200 {:schema s/Any}}`
at the top-level to show responses for all endpoints.

## Examples

### Simple example

* two routes
* swagger-spec served from  `"/swagger.json"`
* swagger-ui mounted to `"/api-docs"`
* note that for real-world use, you need a [content-negotiation middleware][muuntaja] -
  see the next example

[muuntaja]: ../ring/default_middleware.md#content-negotiation

```clj
(require '[reitit.ring :as ring])
(require '[reitit.swagger :as swagger])
(require '[reitit.swagger-ui :as swagger-ui])

(def app
  (ring/ring-handler
    (ring/router
      [["/api"
        ["/ping" {:get (constantly {:status 200, :body "ping"})}]
        ["/pong" {:post (constantly {:status 200, :body "pong"})}]]
       ["" {:no-doc true}
        ["/swagger.json" {:get (swagger/create-swagger-handler)}]
        ["/api-docs/*" {:get (swagger-ui/create-swagger-ui-handler)}]]])))
```

The generated swagger spec:

```clj
(app {:request-method :get :uri "/swagger.json"})
;{:status 200
; :body {:swagger "2.0"
;        :x-id #{:reitit.swagger/default}
;        :paths {"/api/ping" {:get {}}
;                "/api/pong" {:post {}}}}}
```

Swagger-ui:

```clj
(app {:request-method :get, :uri "/api-docs/index.html"})
; ... the swagger-ui index-page, configured correctly
```

You might be interested in adding a [trailing slash handler](slash_handler.md) to the app to serve the swagger-ui from `/api-docs` (without the trailing slash) too.

Another way to serve the swagger-ui is using the [default handler](default_handler.md):

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/api"
        ["/ping" {:get (constantly {:status 200, :body "ping"})}]
        ["/pong" {:post (constantly {:status 200, :body "pong"})}]]
       ["/swagger.json"
        {:get {:no-doc true
               :handler (swagger/create-swagger-handler)}}]])
    (swagger-ui/create-swagger-ui-handler {:path "/api-docs"})))
```

### More complete example

* `clojure.spec` coercion
* swagger data (`:tags`, `:produces`, `:summary`, `:basePath`)
* swagger-spec served from  `"/swagger.json"`
* swagger-ui mounted to `"/"`
* set of middleware for content negotiation, exceptions, multipart etc.
* missed routes are handled by `create-default-handler`
* served via [ring-jetty](https://github.com/ring-clojure/ring/tree/master/ring-jetty-adapter)

Whole example project is in [`/examples/ring-spec-swagger`](https://github.com/metosin/reitit/tree/master/examples/ring-spec-swagger).

```clj
(ns example.server
  (:require [reitit.ring :as ring]
            [reitit.swagger :as swagger]
            [reitit.swagger-ui :as swagger-ui]
            [reitit.ring.coercion :as coercion]
            [reitit.coercion.spec]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [reitit.ring.middleware.exception :as exception]
            [reitit.ring.middleware.multipart :as multipart]
            [reitit.ring.middleware.parameters :as parameters]
            [ring.middleware.params :as params]
            [ring.adapter.jetty :as jetty]
            [muuntaja.core :as m]
            [clojure.java.io :as io]))

(def app
  (ring/ring-handler
    (ring/router
      [["/swagger.json"
        {:get {:no-doc true
               :swagger {:info {:title "my-api"}
                         :basePath "/"} ;; prefix for all paths
               :handler (swagger/create-swagger-handler)}}]

       ["/files"
        {:swagger {:tags ["files"]}}

        ["/upload"
         {:post {:summary "upload a file"
                 :parameters {:multipart {:file multipart/temp-file-part}}
                 :responses {200 {:body {:file multipart/temp-file-part}}}
                 :handler (fn [{{{:keys [file]} :multipart} :parameters}]
                            {:status 200
                             :body {:file file}})}}]

        ["/download"
         {:get {:summary "downloads a file"
                :swagger {:produces ["image/png"]}
                :handler (fn [_]
                           {:status 200
                            :headers {"Content-Type" "image/png"}
                            :body (io/input-stream (io/resource "reitit.png"))})}}]]

       ["/math"
        {:swagger {:tags ["math"]}}

        ["/plus"
         {:get {:summary "plus with spec query parameters"
                :parameters {:query {:x int?, :y int?}}
                :responses {200 {:body {:total int?}}}
                :handler (fn [{{{:keys [x y]} :query} :parameters}]
                           {:status 200
                            :body {:total (+ x y)}})}
          :post {:summary "plus with spec body parameters"
                 :parameters {:body {:x int?, :y int?}}
                 :responses {200 {:body {:total int?}}}
                 :handler (fn [{{{:keys [x y]} :body} :parameters}]
                            {:status 200
                             :body {:total (+ x y)}})}}]]]

      {:data {:coercion reitit.coercion.spec/coercion
              :muuntaja m/instance
              :middleware [;; query-params & form-params
                           parameters/parameters-middleware
                           ;; content-negotiation
                           muuntaja/format-negotiate-middleware
                           ;; encoding response body
                           muuntaja/format-response-middleware
                           ;; exception handling
                           exception/exception-middleware
                           ;; decoding request body
                           muuntaja/format-request-middleware
                           ;; coercing response bodys
                           coercion/coerce-response-middleware
                           ;; coercing request parameters
                           coercion/coerce-request-middleware
                           ;; multipart
                           multipart/multipart-middleware]}})
    (ring/routes
      (swagger-ui/create-swagger-ui-handler {:path "/"})
      (ring/create-default-handler))))

(defn start []
  (jetty/run-jetty #'app {:port 3000, :join? false})
  (println "server running in port 3000"))
```

http://localhost:3000 should render now the swagger-ui:

![Swagger-ui](../images/swagger.png)

## Multiple swagger apis

There can be multiple swagger apis within a router. Each route can be part of 0..n swagger apis. Swagger apis are identified by value in route data under key path `[:swagger :id]`. It can be either a keyword or a sequence of keywords. Normal route data [scoping rules](../basics/route_data.md#nested-route-data) rules apply.

Example with:

* 4 routes
* 2 swagger apis `::one` and `::two`
* 3 swagger specs

```clj
(require '[reitit.ring :as ring])
(require '[reitit.swagger :as swagger])

(def ping-route
  ["/ping" {:get (constantly {:status 200, :body "ping"})}])

(def spec-route
  ["/swagger.json"
   {:get {:no-doc true
          :handler (swagger/create-swagger-handler)}}])

(def app
  (ring/ring-handler
    (ring/router
      [["/common" {:swagger {:id #{::one ::two}}} ping-route]
       ["/one" {:swagger {:id ::one}} ping-route spec-route]
       ["/two" {:swagger {:id ::two}} ping-route spec-route
        ["/deep" {:swagger {:id ::one}} ping-route]]
       ["/one-two" {:swagger {:id #{::one ::two}}} spec-route]])))
```

```clj
(-> {:request-method :get, :uri "/one/swagger.json"} app :body :paths keys)
; ("/common/ping" "/one/ping" "/two/deep/ping")
```

```clj
(-> {:request-method :get, :uri "/two/swagger.json"} app :body :paths keys)
; ("/common/ping" "/two/ping")
```

```clj
(-> {:request-method :get, :uri "/one-two/swagger.json"} app :body :paths keys)
; ("/common/ping" "/one/ping" "/two/ping" "/two/deep/ping")
```

## Reusable schema definitions

Swagger supports having reusable schema definitions under the
`"definitions"` key. These can be reused in different parts of
swagger.json using the `"$ref": "#/definitions/Foo"` syntax. These
definitions are also rendered in their own section in Swagger UI.

Reusable schema objects are generated for Malli `:ref`s and vars.
Currently (as of 0.7.2), reusable schema objects are **not** generated
for Plumatic Schema or Spec.

## TODO

* ClojureScript
  * example for [Macchiato](https://github.com/macchiato-framework)
  * body formatting
  * resource handling

```

`/Users/ovistoica/workspace/reitit/doc/ring/route_data_validation.md`:

```md
# Route Data Validation

Ring route validation works [just like with core router](../basics/route_data_validation.md), with few differences:

* `reitit.ring.spec/validate` should be used instead of `reitit.spec/validate` - to support validating all endpoints (`:get`, `:post` etc.)
* With `clojure.spec` validation, Middleware can contribute to route spec via `:specs` key. The effective route data spec is router spec merged with middleware specs.

## Example

A simple app with spec-validation turned on:

```clj
(require '[clojure.spec.alpha :as s])
(require '[reitit.ring :as ring])
(require '[reitit.ring.spec :as rrs])
(require '[reitit.spec :as rs])
(require '[expound.alpha :as e])

(defn handler [_]
  {:status 200, :body "ok"})

(def app
  (ring/ring-handler
    (ring/router
      ["/api"
       ["/public"
        ["/ping" {:get handler}]]
       ["/internal"
        ["/users" {:get {:handler handler}
                   :delete {:handler handler}}]]]
      {:validate rrs/validate
       ::rs/explain e/expound-str})))
```

All good:

```clj
(app {:request-method :get
      :uri "/api/internal/users"})
; {:status 200, :body "ok"}
```

### Explicit specs via middleware

Middleware that requires `:zone` to be present in route data:

```clj
(s/def ::zone #{:public :internal})

(def zone-middleware
  {:name ::zone-middleware
   :spec (s/keys :req-un [::zone])
   :wrap (fn [handler]
           (fn [request]
             (let [zone (-> request (ring/get-match) :data :zone)]
               (println zone)
               (handler request))))})
```

Missing route data fails fast at router creation:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [zone-middleware]} ;; <--- added
       ["/public"
        ["/ping" {:get handler}]]
       ["/internal"
        ["/users" {:get {:handler handler}
                   :delete {:handler handler}}]]]
      {:validate rrs/validate
       ::rs/explain e/expound-str})))
; CompilerException clojure.lang.ExceptionInfo: Invalid route data:
;
; -- On route -----------------------
;
; "/api/public/ping" :get
;
; -- Spec failed --------------------
;
; {:middleware ...,
;  :handler ...}
;
; should contain key: `:zone`
;
; |   key |  spec |
; |-------+-------|
; | :zone | :zone |
;
;
; -- On route -----------------------
;
; "/api/internal/users" :get
;
; -- Spec failed --------------------
;
; {:middleware ...,
;  :handler ...}
;
; should contain key: `:zone`
;
; |   key |  spec |
; |-------+-------|
; | :zone | :zone |
;
;
; -- On route -----------------------
;
; "/api/internal/users" :delete
;
; -- Spec failed --------------------
;
; {:middleware ...,
;  :handler ...}
;
; should contain key: `:zone`
;
; |   key |  spec |
; |-------+-------|
; | :zone | :zone |
```

Adding the `:zone` to route data fixes the problem:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [zone-middleware]}
       ["/public" {:zone :public} ;; <--- added
        ["/ping" {:get handler}]]
       ["/internal" {:zone :internal} ;; <--- added
        ["/users" {:get {:handler handler}
                   :delete {:handler handler}}]]]
      {:validate rrs/validate
       ::rs/explain e/expound-str})))

(app {:request-method :get
      :uri "/api/internal/users"})
; in zone :internal
; => {:status 200, :body "ok"}
```

### Implicit specs

By design, clojure.spec validates all fully-qualified keys with `s/keys` specs even if they are not defined in that keyset. Validation is implicit but powerful.

Let's reuse the `wrap-enforce-roles` from [Dynamic extensions](dynamic_extensions.md) and define specs for the data:

```clj
(require '[clojure.set :as set])

(s/def ::role #{:admin :manager})
(s/def ::roles (s/coll-of ::role :into #{}))

(defn wrap-enforce-roles [handler]
  (fn [{::keys [roles] :as request}]
    (let [required (some-> request (ring/get-match) :data ::roles)]
      (if (and (seq required) (not (set/subset? required roles)))
        {:status 403, :body "forbidden"}
        (handler request)))))
```

`wrap-enforce-roles` silently ignores if the `::roles` is not present:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [zone-middleware
                            wrap-enforce-roles]} ;; <--- added
       ["/public" {:zone :public}
        ["/ping" {:get handler}]]
       ["/internal" {:zone :internal}
        ["/users" {:get {:handler handler}
                   :delete {:handler handler}}]]]
      {:validate rrs/validate
       ::rs/explain e/expound-str})))

(app {:request-method :get
      :uri "/api/zones/admin/ping"})
; in zone :internal
; => {:status 200, :body "ok"}
```

But fails if they are present and invalid:

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [zone-middleware
                            wrap-enforce-roles]}
       ["/public" {:zone :public}
        ["/ping" {:get handler}]]
       ["/internal" {:zone :internal}
        ["/users" {:get {:handler handler
                         ::roles #{:manager} ;; <--- added
                   :delete {:handler handler
                            ::roles #{:adminz}}}]]] ;; <--- added
      {:validate rrs/validate
       ::rs/explain e/expound-str})))
; CompilerException clojure.lang.ExceptionInfo: Invalid route data:
;
; -- On route -----------------------
;
; "/api/internal/users" :delete
;
; -- Spec failed --------------------
;
; {:middleware ...,
;  :zone ...,
;  :handler ...,
;  :user/roles #{:adminz}}
;                ^^^^^^^
;
; should be one of: `:admin`,`:manager`
```

### Pushing the data to the endpoints

Ability to define (and reuse) route-data in mid-paths is a powerful feature, but having data defined all around might be harder to reason about. There is always an option to define all data at the endpoints.

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api"
       ["/public"
        ["/ping" {:zone :public
                  :get handler
                  :middleware [zone-middleware
                               wrap-enforce-roles]}]]
       ["/internal"
        ["/users" {:zone :internal
                   :middleware [zone-middleware
                                wrap-enforce-roles]
                   :get {:handler handler
                         ::roles #{:manager}}
                   :delete {:handler handler
                            ::roles #{:admin}}}]]]
      {:validate rrs/validate
       ::rs/explain e/expound-str})))
```

Or even flatten the routes:

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/api/public/ping" {:zone :public
                            :get handler
                            :middleware [zone-middleware
                                         wrap-enforce-roles]}]
       ["/api/internal/users" {:zone :internal
                               :middleware [zone-middleware
                                            wrap-enforce-roles]
                               :get {:handler handler
                                     ::roles #{:manager}}
                               :delete {:handler handler
                                        ::roles #{:admin}}}]]
      {:validate rrs/validate
       ::rs/explain e/expound-str})))
```

The common Middleware can also be pushed to the router, here cleanly separating behavior and data:

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/api/public/ping" {:zone :public
                            :get handler}]
       ["/api/internal/users" {:zone :internal
                               :get {:handler handler
                                     ::roles #{:manager}}
                               :delete {:handler handler
                                        ::roles #{:admin}}}]]
      {:data {:middleware [zone-middleware wrap-enforce-roles]}
       :validate rrs/validate
       ::rs/explain e/expound-str})))
```

```

`/Users/ovistoica/workspace/reitit/doc/ring/slash_handler.md`:

```md
# Slash handler

The router works with precise matches. If a route is defined without a trailing slash, for example, it won't match a request with a slash.

```clj
(require '[reitit.ring :as ring])

(def app
  (ring/ring-handler
    (ring/router
      ["/ping" (constantly {:status 200, :body ""})])))

(app {:uri "/ping/"})
; nil
```

Sometimes it is desirable that paths with and without a trailing slash are recognized as the same.

Setting the `redirect-trailing-slash-handler` as a second argument to `ring-handler`:

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/ping" (constantly {:status 200, :body ""})]
       ["/pong/" (constantly {:status 200, :body ""})]])
    (ring/redirect-trailing-slash-handler)))

(app {:uri "/ping/"})
; {:status 308, :headers {"Location" "/ping"}, :body ""}

(app {:uri "/pong"})
; {:status 308, :headers {"Location" "/pong/"}, :body ""}
```

`redirect-trailing-slash-handler` accepts an optional `:method` parameter that allows configuring how (whether) to handle missing/extra slashes. The default is to handle both.

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/ping" (constantly {:status 200, :body ""})]
       ["/pong/" (constantly {:status 200, :body ""})]])
    ; only handle extra trailing slash
    (ring/redirect-trailing-slash-handler {:method :strip})))

(app {:uri "/ping/"})
; {:status 308, :headers {"Location" "/ping"}, :body ""}

(app {:uri "/pong"})
; nil
```

```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/ping" (constantly {:status 200, :body ""})]
       ["/pong/" (constantly {:status 200, :body ""})]])
    ; only handle missing trailing slash
    (ring/redirect-trailing-slash-handler {:method :add})))

(app {:uri "/ping/"})
; nil

(app {:uri "/pong"})
; {:status 308, :headers {"Location" "/pong/"}, :body ""}
```

`redirect-trailing-slash-handler` can be composed with the default handler using `ring/routes` for more correct http error responses:
```clj
(def app
  (ring/ring-handler
    (ring/router
      [["/ping" (constantly {:status 200, :body ""})]
       ["/pong/" (constantly {:status 200, :body ""})]])
    (ring/routes
      (ring/redirect-trailing-slash-handler {:method :add})
      (ring/create-default-handler))))

(app {:uri "/ping/"})
; {:status 404, :body "", :headers {}}

(app {:uri "/pong"})
; {:status 308, :headers {"Location" "/pong/"}, :body ""}
  ```

```

`/Users/ovistoica/workspace/reitit/doc/ring/data_driven_middleware.md`:

```md
# Data-driven Middleware

Ring [defines middleware](https://github.com/ring-clojure/ring/wiki/Concepts#middleware) as a function of type `handler & args => request => response`. It is relatively easy to understand and allows for good performance. A downside is that the middleware chain is just a opaque function, making things like debugging and composition hard. It is too easy to apply the middlewares in wrong order.

Reitit defines middleware as data:

1. A middleware can be defined as first-class data entries
2. A middleware can be mounted as a [duct-style](https://github.com/duct-framework/duct/wiki/Configuration) vector (of middlewares)
4. A middleware can be optimized & [compiled](compiling_middleware.md) against an endpoint
3. A middleware chain can be transformed by the router

## Middleware as data

All values in the `:middleware` vector of route data are expanded into `reitit.middleware/Middleware` Records by using the `reitit.middleware/IntoMiddleware` Protocol. By default, functions, maps and `Middleware` records are allowed.

Records can have arbitrary keys, but the following keys have special purpose:

| key            | description |
| ---------------|-------------|
| `:name`        | Name of the middleware as a qualified keyword
| `:spec`        | `clojure.spec` definition for the route data, see [route data validation](route_data_validation.md) (optional)
| `:wrap`        | The actual middleware function of `handler & args => request => response`
| `:compile`     | Middleware compilation function, see [compiling middleware](compiling_middleware.md).

Middleware Records are accessible in their raw form in the compiled route results, and thus are available for inventories, creating api-docs, etc.

For the actual request processing, the Records are unwrapped into normal functions and composed into a middleware function chain, yielding zero runtime penalty.

### Creating Middleware

The following examples produce identical middleware runtime functions.

### Function

```clj
(defn wrap [handler id]
  (fn [request]
    (handler (update request ::acc (fnil conj []) id))))
```

### Map

```clj
(def wrap3
  {:name ::wrap3
   :description "Middleware that does things."
   :wrap wrap})
```

### Record

```clj
(require '[reitit.middleware :as middleware])

(def wrap2
  (middleware/map->Middleware
    {:name ::wrap2
     :description "Middleware that does things."
     :wrap wrap}))
```

## Using Middleware

`:middleware` is merged to endpoints by the `router`.

```clj
(require '[reitit.ring :as ring])

(defn handler [{::keys [acc]}]
  {:status 200, :body (conj acc :handler)})

(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[wrap 1] [wrap2 2]]}
       ["/ping" {:get {:middleware [[wrap3 3]]
                       :handler handler}}]])))
```

All the middlewares are applied correctly:

```clj
(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body [1 2 3 :handler]}
```

## Compiling middleware

Middlewares can be optimized against an endpoint using [middleware compilation](compiling_middleware.md).

## Ideas for the future

* Support Middleware dependency resolution with new keys `:requires` and `:provides`. Values are set of top-level keys of the request. e.g.
   * `InjectUserIntoRequestMiddleware` requires `#{:session}` and provides `#{:user}`
   * `AuthorizationMiddleware` requires `#{:user}`

Ideas welcome & see [issues](https://github.com/metosin/reitit/issues) for details.

```

`/Users/ovistoica/workspace/reitit/doc/ring/transforming_middleware_chain.md`:

```md
# Transforming the Middleware Chain

There is an extra option in the Ring router (actually, in the underlying middleware-router): `:reitit.middleware/transform` to transform the middleware chain per endpoint. Value should be a function or a vector of functions that get a vector of compiled middleware and should return a new vector of middleware.

## Example Application

```clj
(require '[reitit.ring :as ring])
(require '[reitit.middleware :as middleware])

(defn wrap [handler id]
  (fn [request]
    (handler (update request ::acc (fnil conj []) id))))

(defn handler [{::keys [acc]}]
  {:status 200, :body (conj acc :handler)})

(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[wrap 1] [wrap 2]]}
       ["/ping" {:get {:middleware [[wrap 3]]
                       :handler handler}}]])))

(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body [1 2 3 :handler]}
```

### Reversing the Middleware Chain

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[wrap 1] [wrap 2]]}
       ["/ping" {:get {:middleware [[wrap 3]]
                       :handler handler}}]]
      {::middleware/transform reverse})))

(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body [3 2 1 :handler]}
```

## Interleaving Middleware

```clj
(def app
  (ring/ring-handler
    (ring/router
      ["/api" {:middleware [[wrap 1] [wrap 2]]}
       ["/ping" {:get {:middleware [[wrap 3]]
                       :handler handler}}]]
      {::middleware/transform #(interleave % (repeat [wrap :debug]))})))

(app {:request-method :get, :uri "/api/ping"})
; {:status 200, :body [1 :debug 2 :debug 3 :debug :handler]}
```

### Printing Request Diffs

```clj
[metosin/reitit-middleware "0.8.0"]
```

Using `reitit.ring.middleware.dev/print-request-diffs` transformation, the request diffs between each middleware are printed out to the console. To use it, add the following router option:

```clj
:reitit.middleware/transform reitit.ring.middleware.dev/print-request-diffs
```

Sample output:

![Ring Request Diff](../images/ring-request-diff.png)

```

`/Users/ovistoica/workspace/reitit/doc/ring/reverse_routing.md`:

```md
# Reverse routing with Ring

Both the `router` and the `match` are injected into Ring Request (as `::r/router` and `::r/match`) by the `reitit.ring/ring-handler` and with that, available to middleware and endpoints. To convert a `Match` into a path, one can use `r/match->path`, which optionally takes a map of query-parameters too.

Below is an example how to do reverse routing from a ring handler:

```clj
(require '[reitit.core :as r])
(require '[reitit.ring :as ring])

(def app
  (ring/ring-handler
    (ring/router
      [["/users"
        {:get (fn [{::r/keys [router]}]
                {:status 200
                 :body (for [i (range 10)]
                         {:uri (-> router
                                   (r/match-by-name ::user {:id i})
                                   ;; with extra query-params
                                   (r/match->path {:iso "möly"}))})})}]
       ["/users/:id"
        {:name ::user
         :get (constantly {:status 200, :body "user..."})}]])))

(app {:request-method :get, :uri "/users"})
; {:status 200,
;  :body ({:uri "/users/0?iso=m%C3%B6ly"}
;         {:uri "/users/1?iso=m%C3%B6ly"}
;         {:uri "/users/2?iso=m%C3%B6ly"}
;         {:uri "/users/3?iso=m%C3%B6ly"}
;         {:uri "/users/4?iso=m%C3%B6ly"}
;         {:uri "/users/5?iso=m%C3%B6ly"}
;         {:uri "/users/6?iso=m%C3%B6ly"}
;         {:uri "/users/7?iso=m%C3%B6ly"}
;         {:uri "/users/8?iso=m%C3%B6ly"}
;         {:uri "/users/9?iso=m%C3%B6ly"})}
```

```

`/Users/ovistoica/workspace/reitit/doc/ring/openapi.md`:

```md
# OpenAPI Support

**Stability: alpha**

Reitit can generate [OpenAPI 3.1.0](https://spec.openapis.org/oas/v3.1.0)
documentation. The feature works similarly to [Swagger documentation](swagger.md).

The main example is [examples/openapi](../../examples/openapi).
The
[ring-malli-swagger](../../examples/ring-malli-swagger)
and
[ring-spec-swagger](../../examples/ring-spec-swagger)
examples also
have OpenAPI documentation.

## OpenAPI data

The following route data keys contribute to the generated swagger specification:

| key            | description |
| ---------------|-------------|
| :openapi       | map of any openapi data. Can contain keys like `:deprecated`.
| :no-doc        | optional boolean to exclude endpoint from api docs
| :tags          | optional set of string or keyword tags for an endpoint api docs
| :summary       | optional short string summary of an endpoint
| :description   | optional long description of an endpoint. Supports http://spec.commonmark.org/
| :openapi/request-content-types | See the Per-content-type-coercions section below.
| :openapi/response-content-types |See the Per-content-type-coercions section below. vector of supported response content types. Defaults to `["application/json"]`. Only needed if you use the [:response nnn :content :default] coercion.

Coercion keys also contribute to the docs:

| key           | description |
| --------------|-------------|
| :parameters   | optional input parameters for a route, in a format defined by the coercion
| :request      | optional description of body parameters, possibly per content-type
| :responses    | optional descriptions of responses, in a format defined by coercion


## Per-content-type coercions

Use `:request` coercion (instead of `:body`) to unlock
per-content-type coercions. This also lets you specify multiple named
examples. See [Coercion](coercion.md) for more info. See also [the
openapi example](../../examples/openapi).

```clj
["/pizza"
 {:get {:summary "Fetch a pizza | Multiple content-types, multiple examples"
        :responses {200 {:description "Fetch a pizza as json or EDN"
                         :content {"application/json" {:schema [:map
                                                                [:color :keyword]
                                                                [:pineapple :boolean]]
                                                       :examples {:white {:description "White pizza with pineapple"
                                                                          :value {:color :white
                                                                                  :pineapple true}}
                                                                  :red {:description "Red pizza"
                                                                        :value {:color :red
                                                                                :pineapple false}}}}
                                   "application/edn" {:schema [:map
                                                               [:color :keyword]
                                                               [:pineapple :boolean]]
                                                      :examples {:red {:description "Red pizza with pineapple"
                                                                       :value (pr-str {:color :red :pineapple true})}}}}}}
```

The special `:default` content types map to the content types supported by the Muuntaja
instance. You can override these by using the `:openapi/request-content-types`
and `:openapi/response-content-types` keys, which must contain vector of
supported content types. If there is no Muuntaja instance, and these keys are
not defined, the content types will default to `["application/json"]`.

## OpenAPI spec

Serving the OpenAPI specification is handled by
`reitit.openapi/create-openapi-handler`. It takes no arguments and returns a
ring handler which collects at request-time data from all routes and returns an
OpenAPI specification as Clojure data, to be encoded by a response formatter.

You can use the `:openapi` route data key of the `create-openapi-handler` route
to populate the top level of the OpenAPI spec.

Example:

```
["/openapi.json"
 {:get {:handler (openapi/create-openapi-handler)
        :openapi {:info {:title "my nice api" :version "0.0.1"}}
        :no-doc true}}]
```

If you need to post-process the generated spec, just wrap the handler with a custom `Middleware` or an `Interceptor`.

## Swagger-ui

[Swagger-UI](https://github.com/swagger-api/swagger-ui) is a user interface to visualize and interact with the Swagger specification. To make things easy, there is a pre-integrated version of the swagger-ui as a separate module. See `reitit.swagger-ui/create-swagger-ui-handle`

## Finetuning the OpenAPI output

There are a number of ways you can specify extra data that gets
included in the OpenAPI spec.

### Custom OpenAPI data

The `:openapi` route data key can be used to add top-level or
route-level information to the generated OpenAPI spec.

A straightforward use case is adding `"externalDocs"`:

```clj
["/account"
 {:get {:summary "Fetch an account | Recursive schemas using malli registry, link to external docs"
        :openapi {:externalDocs {:description "The reitit repository"
                                 :url "https://github.com/metosin/reitit"}}
        ...}}]
```

In a more complex use case is providing `"securitySchemes"`. See
[the openapi example](../../examples/openapi) for a working example of
`"securitySchemes"`. See also the
[OpenAPI docs](https://spec.openapis.org/oas/v3.1.0.html#security-scheme-object)

### Annotating schemas

You can use malli properties, schema-tools data or spec-tools data to
annotate your models with examples, descriptions and defaults that
show up in the OpenAPI spec.

This approach lets you add additional keys to the
[OpenAPI Schema Objects](https://spec.openapis.org/oas/v3.1.0.html#schema-object).
The most common ones are default and example values for parameters.

Malli:

```clj
["/plus"
 {:post
  {:parameters
   {:body [:map
           [:x
            {:title "X parameter"
             :description "Description for X parameter"
             :json-schema/default 42}
            int?]
           [:y int?]]}}}]
```

Schema:

```clj
["/plus"
 {:post
  {:parameters
   {:body {:x (schema-tools.core/schema s/Num {:description "Description for X parameter"
                                               :openapi/example 13
                                               :openapi/default 42})
           :y int?}}}}]
```

Spec:

```clj
["/plus"
 {:post
  {:parameters
   {:body (spec-tools.data-spec/spec ::foo
                                     {:x (schema-tools.core/spec {:spec int?
                                                                  :description "Description for X parameter"
                                                                  :openapi/example 13
                                                                  :openapi/default 42})
                                      :y int?}}}}}]
```

### Adding examples

Adding request/response examples have been mentioned above a couple of times
above. Here's a summary of the different ways to do it:

1. Add an example to the schema object using a `:openapi/example`
   (schema, spec) or `:json-schema/example` (malli) key in your
   schema/spec/malli model metadata. See the examples above.
2. Use `:example` (a single example) or `:examples` (named examples)
   with per-content-type coercion.

**Caveat!** When adding examples for query parameters (or headers),
you must add the examples to the individual parameters, not the map
schema surrounding them. This is due to limitations in how OpenAPI
represents query parameters.

```clj
;; Wrong!
{:parameters {:query [:map
                      {:json-schema/example {:a 1}}
                      [:a :int]]}}
;; Right!
{:parameters {:query [:map
                      [:a {:json-schema/example 1} :int]]}}
```

### Named schemas

OpenAPI supports reusable schema objects that can be referred to with
the `"$ref": "#/components/schemas/Foo"` json-schema syntax. This is
useful when you have multiple endpoints that use the same schema. It
can also make OpenAPI-based code nicer for consumers of your API.
These schemas are also rendered in their own section in Swagger UI.

Reusable schema objects are generated for Malli `:ref`s and vars. The
[openapi example](../../examples/openapi) showcases this.

Currently (as of 0.7.2), reusable schema objects are **not** generated
for Plumatic Schema or Spec.

```

`/Users/ovistoica/workspace/reitit/doc/ring/compiling_middleware.md`:

```md
# Compiling Middleware

The [dynamic extensions](dynamic_extensions.md) are an easy way to extend the system. To enable fast lookup of route data, we can compile them into any shape (records, functions etc.), enabling fast access at request-time.

But, we can do much better. As we know the exact route that a middleware/interceptor is linked to, we can pass the (compiled) route information into the middleware at creation-time. It can do local reasoning: Extract and transform relevant data just for it and pass the optimized data into the actual request-handler via a closure - yielding much faster runtime processing. A middleware can also decide not to mount itself by returning `nil`. (E.g. Why mount a `wrap-enforce-roles` middleware for a route if there are no roles required for it?)

To enable this we use [middleware records](data_driven_middleware.md) `:compile` key instead of the normal `:wrap`. `:compile` expects a function of `route-data router-opts => ?IntoMiddleware`.

To demonstrate the two approaches, below is the response coercion middleware written as normal ring middleware function and as middleware record with `:compile`.

## Normal Middleware

* Reads the compiled route information on every request. Everything is done at request-time.

```clj
(defn wrap-coerce-response
  "Middleware for pluggable response coercion.
  Expects a :coercion of type `reitit.coercion/Coercion`
  and :responses from route data, otherwise will do nothing."
  [handler]
  (fn
    ([request]
     (let [response (handler request)
           method (:request-method request)
           match (ring/get-match request)
           responses (-> match :result method :data :responses)
           coercion (-> match :data :coercion)
           opts (-> match :data :opts)]
       (if (and coercion responses)
         (let [coercers (response-coercers coercion responses opts)]
           (coerce-response coercers request response))
         response)))
    ([request respond raise]
     (let [method (:request-method request)
           match (ring/get-match request)
           responses (-> match :result method :data :responses)
           coercion (-> match :data :coercion)
           opts (-> match :data :opts)]
       (if (and coercion responses)
         (let [coercers (response-coercers coercion responses opts)]
           (handler request #(respond (coerce-response coercers request %))))
         (handler request respond raise))))))
```

## Compiled Middleware

* Route information is provided at creation-time
* Coercers are compiled at creation-time
* Middleware mounts only if `:coercion` and `:responses` are defined for the route
* Also defines spec for the route data `:responses` for the [route data validation](route_data_validation.md).

```clj
(require '[reitit.spec :as rs])

(def coerce-response-middleware
  "Middleware for pluggable response coercion.
  Expects a :coercion of type `reitit.coercion/Coercion`
  and :responses from route data, otherwise does not mount."
  {:name ::coerce-response
   :spec ::rs/responses
   :compile (fn [{:keys [coercion responses]} opts]
              (if (and coercion responses)
                (let [coercers (coercion/response-coercers coercion responses opts)]
                  (fn [handler]
                    (fn
                      ([request]
                       (coercion/coerce-response coercers request (handler request)))
                      ([request respond raise]
                       (handler request #(respond (coercion/coerce-response coercers request %)) raise)))))))})
```

It has 50% less code, it's much easier to reason about and is much faster.

### Require Keys on Routes at Creation Time

Often it is useful to require a route to provide a specific key.

```clj
(require '[buddy.auth.accessrules :as accessrules])

(s/def ::authorize
  (s/or :handler :accessrules/handler :rule :accessrules/rule))

(def authorization-middleware
  {:name ::authorization
   :spec (s/keys :req-un [::authorize])
   :compile
   (fn [route-data _opts]
     (when-let [rule (:authorize route-data)]
       (fn [handler]
         (accessrules/wrap-access-rules handler {:rules [rule]}))))})
```

In the example above the `:spec` expresses that each route is required to provide the `:authorize` key. However, in this case the compile function returns `nil` when that key is missing, which means **the middleware will not be mounted, the spec will not be considered, and the compiler will not enforce this requirement as intended**.

If you just want to enforce the spec return a map without `:wrap` or `:compile` keys, e.g. an empty map, `{}`.


```clj
(def authorization-middleware
  {:name ::authorization
   :spec (s/keys :req-un [::authorize])
   :compile
   (fn [route-data _opts]
     (if-let [rule (:authorize route-data)]
       (fn [handler]
         (accessrules/wrap-access-rules handler {:rules [rule]}))
       ;; return empty map just to enforce spec
       {}))})
```

The middleware (and associated spec) will still be part of the chain, but will not process the request.

```

`/Users/ovistoica/workspace/reitit/doc/development.md`:

```md
# Development Instructions

## Building

```bash
./scripts/lein-modules do clean, install
```

## Running tests

```bash
./scripts/test.sh clj
./scripts/test.sh cljs
```

## Formatting

```bash
clojure-lsp format
clojure-lsp clean-ns
```

## Documentation

The documentation lives under `doc` and it is hosted on [cljdoc](https://cljdoc.org). See their
documentation for [library authors](https://github.com/cljdoc/cljdoc/blob/master/doc/userguide/for-library-authors.adoc)

## Making a release

We use [Break Versioning][breakver]. Remember our promise: patch-level bumps never include breaking changes!

[breakver]: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md

```bash
# Check that you're using Java 8! Making the release with a newer Java version
# means that it is broken when used with Java 8.
java -version

# new version
./scripts/set-version "1.0.0"

# create a release commit and a tag
git add -u
git commit -m "Release 1.0.0"
git tag 1.0.0

# works
./scripts/lein-modules install
lein test

# deploy to clojars
CLOJARS_USERNAME=*** CLOJARS_PASSWORD=*** ./scripts/lein-modules do clean, deploy clojars

# push the commit and the tag
git push
git push --tags
```

* Remembor to update the changelog!
* Announce the release at least on #reitit in Clojurians.

```

`/Users/ovistoica/workspace/reitit/doc/coercion/schema_coercion.md`:

```md
# Plumatic Schema Coercion

[Plumatic Schema](https://github.com/plumatic/schema) is a Clojure(Script) library for declarative data description and validation.

```clj
(require '[reitit.coercion.schema])
(require '[reitit.coercion :as coercion])
(require '[schema.core :as s])
(require '[reitit.core :as r])

(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.schema/coercion
                                 :parameters {:path {:company s/Str
                                                     :user-id s/Int}}}]
    {:compile coercion/compile-request-coercers}))

(defn match-by-path-and-coerce! [path]
  (if-let [match (r/match-by-path router path)]
    (assoc match :parameters (coercion/coerce! match))))
```

Successful coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view,
;               :coercion <<:schema>>
;               :parameters {:path {:company java.lang.String,
;                                   :user-id Int}}},
;        :result {:path #object[reitit.coercion$request_coercer$]},
;        :path-params {:company "metosin", :user-id "123"},
;        :parameters {:path {:company "metosin", :user-id 123}}
;        :path "/metosin/users/123"}
```

Failing coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/ikitommi")
; => ExceptionInfo Request coercion failed...
```

```

`/Users/ovistoica/workspace/reitit/doc/coercion/coercion.md`:

```md
# Coercion Explained

Coercion is a process of transforming parameters (and responses) from one format into another. Reitit separates routing and coercion into two separate steps.

By default, all wildcard and catch-all parameters are parsed into strings:

```clj
(require '[reitit.core :as r])

(def router
  (r/router
    ["/:company/users/:user-id" ::user-view]))
```

Match with the parsed `:path-params` as strings:

```clj
(r/match-by-path router "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view},
;        :result nil,
;        :path-params {:company "metosin", :user-id "123"},
;        :path "/metosin/users/123"}
```

To enable parameter coercion, the following things need to be done:

1. Define a `Coercion` for the routes
2. Define types for the parameters
3. Compile coercers for the types
4. Apply the coercion

## Define Coercion

`reitit.coercion/Coercion` is a protocol defining how types are defined, coerced and inventoried.

Reitit ships with the following coercion modules:

* `reitit.coercion.malli/coercion` for [malli](https://github.com/metosin/malli)
* `reitit.coercion.schema/coercion` for [plumatic schema](https://github.com/plumatic/schema)
* `reitit.coercion.spec/coercion` for both [clojure.spec](https://clojure.org/about/spec) and [data-specs](https://github.com/metosin/spec-tools#data-specs)

Coercion can be attached to route data under `:coercion` key. There can be multiple `Coercion` implementations within a single router, normal [scoping rules](../basics/route_data.md#nested-route-data) apply.

## Defining parameters

Route parameters can be defined via route data `:parameters`. It has keys for different type of parameters: `:query`, `:body`, `:form`, `:header` and `:path`. Syntax for the actual parameters depends on the `Coercion` implementation.

Example with Schema path-parameters:

```clj
(require '[reitit.coercion.schema])
(require '[schema.core :as s])

(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.schema/coercion
                                 :parameters {:path {:company s/Str
                                                     :user-id s/Int}}}]))
```

A Match:

```clj
(r/match-by-path router "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view,
;               :coercion <<:schema>>
;               :parameters {:path {:company java.lang.String,
;                                   :user-id Int}}},
;        :result nil,
;        :path-params {:company "metosin", :user-id "123"},
;        :path "/metosin/users/123"}
```

Coercion was not applied. Why? In Reitit, routing and coercion are separate processes and we have done just the routing part. We need to apply coercion after the successful routing.

But now we should have enough data on the match to apply the coercion.

## Compiling coercers

Before the actual coercion, we ~~should~~ need to compile the coercers against the route data. Compiled coercers yield much better performance and the manual step of adding a coercion compiler makes things explicit and non-magical.

Compiling can be done via a Middleware, Interceptor or a Router. We apply it now at router-level, effecting all routes (with `:parameters` and `:coercion` defined).

There is a helper function `reitit.coercion/compile-request-coercers` just for this:

```clj
(require '[reitit.coercion :as coercion])
(require '[reitit.coercion.schema])
(require '[schema.core :as s])

(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.schema/coercion
                                 :parameters {:path {:company s/Str
                                                     :user-id s/Int}}}]
    {:compile coercion/compile-request-coercers}))
```

Routing again:

```clj
(r/match-by-path router "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view,
;               :coercion <<:schema>>
;               :parameters {:path {:company java.lang.String,
;                                   :user-id Int}}},
;        :result {:path #object[reitit.coercion$request_coercer$]},
;        :path-params {:company "metosin", :user-id "123"},
;        :path "/metosin/users/123"}
```

The compiler added a `:result` key into the match (done just once, at router creation time), which holds the compiled coercers. We are almost done.

## Applying coercion

We can use a helper function `reitit.coercion/coerce!` to do the actual coercion, based on a `Match`:

```clj
(coercion/coerce!
  (r/match-by-path router "/metosin/users/123"))
; {:path {:company "metosin", :user-id 123}}
```

We get the coerced parameters back. If a coercion fails, a typed (`:reitit.coercion/request-coercion`) ExceptionInfo is thrown, with data about the actual error:

```clj
(coercion/coerce!
  (r/match-by-path router "/metosin/users/ikitommi"))
; => ExceptionInfo Request coercion failed:
; #CoercionError{:schema {:company java.lang.String, :user-id Int, Any Any},
;                :errors {:user-id (not (integer? "ikitommi"))}}
; clojure.core/ex-info (core.clj:4739)
```

## Full example

Here's a full example for doing routing and coercion with Reitit and Schema:

```clj
(require '[reitit.coercion.schema])
(require '[reitit.coercion :as coercion])
(require '[reitit.core :as r])
(require '[schema.core :as s])

(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.schema/coercion
                                 :parameters {:path {:company s/Str
                                                     :user-id s/Int}}}]
    {:compile coercion/compile-request-coercers}))

(defn match-by-path-and-coerce! [path]
  (if-let [match (r/match-by-path router path)]
    (assoc match :parameters (coercion/coerce! match))))

(match-by-path-and-coerce! "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view,
;               :coercion <<:schema>>
;               :parameters {:path {:company java.lang.String,
;                                   :user-id Int}}},
;        :result {:path #object[reitit.coercion$request_coercer$]},
;        :path-params {:company "metosin", :user-id "123"},
;        :parameters {:path {:company "metosin", :user-id 123}}
;        :path "/metosin/users/123"}

(match-by-path-and-coerce! "/metosin/users/ikitommi")
; => ExceptionInfo Request coercion failed...
```

## Ring Coercion

For a full-blown http-coercion, see the [ring coercion](../ring/coercion.md).

```

`/Users/ovistoica/workspace/reitit/doc/coercion/clojure_spec_coercion.md`:

```md
# Clojure.spec Coercion

The [clojure.spec](https://clojure.org/guides/spec) library specifies the structure of data, validates or destructures it, and can generate data based on the spec.

## Warning

`clojure.spec` by itself doesn't support coercion. `reitit` uses [spec-tools](https://github.com/metosin/spec-tools) that adds coercion to spec. Like `clojure.spec`, it's alpha as it leans both on spec walking and `clojure.spec.alpha/conform`, which is considered a spec internal, that might be changed or removed later.

## Usage

For simple specs (core predicates, `spec-tools.core/spec`, `s/and`, `s/or`, `s/coll-of`, `s/keys`, `s/map-of`, `s/nillable` and `s/every`), the transformation is inferred using [spec-walker](https://github.com/metosin/spec-tools#spec-walker) and is automatic. To support all specs (like regex-specs), specs need to be wrapped into [Spec Records](https://github.com/metosin/spec-tools/blob/master/README.md#spec-records).

There are [CLJ-2116](https://clojure.atlassian.net/browse/CLJ-2116) and [CLJ-2251](https://clojure.atlassian.net/browse/CLJ-2251) that would help solve this elegantly. Go vote 'em up.

## Example

```clj
(require '[reitit.coercion.spec])
(require '[reitit.coercion :as coercion])
(require '[spec-tools.spec :as spec])
(require '[clojure.spec.alpha :as s])
(require '[reitit.core :as r])

;; simple specs, inferred
(s/def ::company string?)
(s/def ::user-id int?)
(s/def ::path-params (s/keys :req-un [::company ::user-id]))

(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.spec/coercion
                                 :parameters {:path ::path-params}}]
    {:compile coercion/compile-request-coercers}))

(defn match-by-path-and-coerce! [path]
  (if-let [match (r/match-by-path router path)]
    (assoc match :parameters (coercion/coerce! match))))
```

Successful coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view,
;               :coercion <<:spec>>
;               :parameters {:path ::path-params}},
;        :result {:path #object[reitit.coercion$request_coercer$]},
;        :path-params {:company "metosin", :user-id "123"},
;        :parameters {:path {:company "metosin", :user-id 123}}
;        :path "/metosin/users/123"}
```

Failing coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/ikitommi")
; => ExceptionInfo Request coercion failed...
```

## Deeply nested

Spec-tools allow deeply nested specs to be coerced. One can test the coercion easily in the REPL.

Define some specs:

```clj
(require '[clojure.spec.alpha :as s])
(require '[spec-tools.core :as st])

(s/def :sku/id keyword?)
(s/def ::sku (s/keys :req-un [:sku/id]))
(s/def ::skus (s/coll-of ::sku :into []))

(s/def :photo/id int?)
(s/def ::photo (s/keys :req-un [:photo/id]))
(s/def ::photos (s/coll-of ::photo :into []))

(s/def ::my-json-api (s/keys :req-un [::skus ::photos]))
```

Apply a string->edn coercion to the data:

```clj
(st/coerce
  ::my-json-api
  {:skus [{:id "123"}]
   :photos [{:id "123"}]}
  st/string-transformer)
; {:skus [{:id :123}]
;  :photos [{:id 123}]}
```

Apply a json->edn coercion to the data:

```clj
(st/coerce
  ::my-json-api
  {:skus [{:id "123"}]
   :photos [{:id "123"}]}
  st/json-transformer)
; {:skus [{:id :123}]
;  :photos [{:id "123"}]}
```

By default, reitit uses custom transformers that also strip out extra keys from `s/keys` specs:

```clj
(require '[reitit.coercion.spec :as rcs])

(st/coerce
  ::my-json-api
  {:TOO "MUCH"
   :skus [{:id "123"
           :INFOR "MATION"}]
   :photos [{:id "123"
             :HERE "TOO"}]}
  rcs/json-transformer)
; {:skus [{:id :123}]
;  :photos [{:id "123"}]}
```

## Defining Optional Keys

Going back to the previous example.

Suppose you want the `::my-json-api` to have optional `remarks` as string and each `photo` to have an optional `height` and `width` as integer.
The `s/keys` accepts `:opt-un` to support optional keys.

```clj
(require '[clojure.spec.alpha :as s])
(require '[spec-tools.core :as st])

(s/def :sku/id keyword?)
(s/def ::sku (s/keys :req-un [:sku/id]))
(s/def ::skus (s/coll-of ::sku :into []))
(s/def ::remarks string?)  ;; define remarks as string

(s/def :photo/id int?)
(s/def :photo/height int?) ;; define height as int
(s/def :photo/width int?)  ;; define width as int
(s/def ::photo (s/keys :req-un [:photo/id]
                       :opt-un [:photo/height :photo/width])) ;; height and width are in :opt-un
(s/def ::photos (s/coll-of ::photo :into []))

(s/def ::my-json-api (s/keys :req-un [::skus ::photos]
                             :opt-un [::remarks])) ;; remarks is in the :opt-un
```

Apply a string->edn coercion to the data:

```clj
;; Omit optional keys
(st/coerce
  ::my-json-api
  {:skus [{:id "123"}]
   :photos [{:id "123"}]}
  st/string-transformer)
;;{:skus [{:id :123}],
;; :photos [{:id 123}]}


;; coerce the optional keys if present

(st/coerce
  ::my-json-api
  {:skus [{:id "123"}]
   :photos [{:id "123" :height "100" :width "100"}]
   :remarks "some remarks"}
  st/string-transformer)

;; {:skus [{:id :123}]
;;  :photos [{:id 123 :height 100 :width 100}]
;;  :remarks "some remarks"}

(st/coerce
  ::my-json-api
  {:skus [{:id "123"}]
   :photos [{:id "123" :height "100"}]}
  st/string-transformer)
;; {:skus [{:id :123}],
;;  :photos [{:id 123, :height 100}]}
```

```

`/Users/ovistoica/workspace/reitit/doc/coercion/malli_coercion.md`:

```md
# Malli Coercion

[Malli](https://github.com/metosin/malli) is data-driven Schema library for Clojure/Script.

## Default Syntax

By default, [Vector Syntax](https://github.com/metosin/malli#vector-syntax) is used:

```clj
(require '[reitit.coercion.malli])
(require '[reitit.coercion :as coercion])
(require '[reitit.core :as r])

(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.malli/coercion
                                 :parameters {:path [:map
                                                     [:company string?]
                                                     [:user-id int?]]}}]
    {:compile coercion/compile-request-coercers}))

(defn match-by-path-and-coerce! [path]
  (if-let [match (r/match-by-path router path)]
    (assoc match :parameters (coercion/coerce! match))))
```

Successful coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view,
;               :coercion <<:malli>>
;               :parameters {:path [:map
;                                   [:company string?]
;                                   [:user-id int?]]}},
;        :result {:path #object[reitit.coercion$request_coercer$]},
;        :path-params {:company "metosin", :user-id "123"},
;        :parameters {:path {:company "metosin", :user-id 123}}
;        :path "/metosin/users/123"}
```

Failing coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/ikitommi")
; => ExceptionInfo Request coercion failed...
```

## Lite Syntax

Same using [Lite Syntax](https://github.com/metosin/malli#lite):

```clj
(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.malli/coercion
                                 :parameters {:path {:company string?
                                                     :user-id int?}}}]
    {:compile coercion/compile-request-coercers}))
```

## Configuring coercion

Using `create` with options to create the coercion instead of `coercion`:

```clj
(require '[malli.util :as mu])

(reitit.coercion.malli/create
  {:transformers {:body {:default reitit.coercion.malli/default-transformer-provider
                         :formats {"application/json" reitit.coercion.malli/json-transformer-provider}}
                  :string {:default reitit.coercion.malli/string-transformer-provider}
                  :response {:default reitit.coercion.malli/default-transformer-provider}}
   ;; set of keys to include in error messages
   :error-keys #{:type :coercion :in :schema :value :errors :humanized #_:transformed}
   ;; support lite syntax?
   :lite true
   ;; schema identity function (default: close all map schemas)
   :compile mu/closed-schema
   ;; validate request & response
   :validate true
   ;; top-level short-circuit to disable request & response coercion
   :enabled true
   ;; strip-extra-keys (affects only predefined transformers)
   :strip-extra-keys true
   ;; add/set default values
   :default-values true
   ;; malli options
   :options nil})
```

```

`/Users/ovistoica/workspace/reitit/doc/coercion/data_spec_coercion.md`:

```md
# Data-spec Coercion

[Data-specs](https://github.com/metosin/spec-tools#data-specs) is alternative, macro-free syntax to define `clojure.spec`s. As a bonus, supports the [runtime transformations via conforming](https://clojure.atlassian.net/browse/CLJ-2116) out-of-the-box.

```clj
(require '[reitit.coercion.spec])
(require '[reitit.coercion :as coercion])
(require '[reitit.core :as r])

(def router
  (r/router
    ["/:company/users/:user-id" {:name ::user-view
                                 :coercion reitit.coercion.spec/coercion
                                 :parameters {:path {:company string?
                                                     :user-id int?}}}]
    {:compile coercion/compile-request-coercers}))

(defn match-by-path-and-coerce! [path]
  (if-let [match (r/match-by-path router path)]
    (assoc match :parameters (coercion/coerce! match))))
```

Successful coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/123")
; #Match{:template "/:company/users/:user-id",
;        :data {:name :user/user-view,
;               :coercion <<:spec>>
;               :parameters {:path {:company string?,
;                                   :user-id int?}}},
;        :result {:path #object[reitit.coercion$request_coercer$]},
;        :path-params {:company "metosin", :user-id "123"},
;        :parameters {:path {:company "metosin", :user-id 123}}
;        :path "/metosin/users/123"}
```

Failing coercion:

```clj
(match-by-path-and-coerce! "/metosin/users/ikitommi")
; => ExceptionInfo Request coercion failed...
```

```
