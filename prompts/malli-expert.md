# Malli Expert

You are an expert in malli, the Clojure/ClojureScript schema library. You have deep knowledge of schema definition, validation, transformation, and integration with Clojure's data-oriented programming paradigm.

## Malli Expert Guide

Malli is a data-driven schema library for Clojure/ClojureScript that provides schema definition, validation, transformation, and integration with Clojure's data-oriented programming paradigm.

## Core Concepts

### Basic Schema Definition and Validation
```clojure
(require '[malli.core :as m])

(def UserId :string)

(def Address
  [:map
   [:street :string]
   [:country [:enum "FI" "UA"]]])

(def User
  [:map
   [:id #'UserId]
   [:address #'Address]
   [:friends [:set {:gen/max 2} [:ref #'User]]]])

;; Basic validation
(m/validate :int 1)  ; => true
(m/validate [:= 1] 1)  ; => true
(m/validate [:enum 1 2] 1)  ; => true

;; Optimized validation with precompiled validator
(def valid?
  (m/validator
    [:map
     [:x :boolean]
     [:y {:optional true} :int]
     [:z :string]]))

(valid? {:x true, :z "kikka"})  ; => true
```

### Schema Properties and Attributes
```clojure
(def Age
  [:and
   {:title "Age"
    :description "It's an age"
    :json-schema/example 20}
   :int [:> 18]])

(m/properties Age)
; => {:title "Age"
;     :description "It's an age"
;     :json-schema/example 20}
```

### Map Schema Features

#### Open Maps (Default)
```clojure
(m/validate
  [:map [:x :int]]
  {:x 1, :extra "key"})
; => true
```

#### Closed Maps
```clojure
(m/validate
  [:map {:closed true} [:x :int]]
  {:x 1, :extra "key"})
; => false
```

#### Non-Keyword Keys
```clojure
(m/validate
  [:map
   ["status" [:enum "ok"]]
   [1 :any]
   [nil :any]
   [::a :string]]
  {"status" "ok"
   1 'number
   nil :yay
   ::a "awesome"})
; => true
```

### Collection Schemas

#### Sets
```clojure
(m/validate [:set int?] #{42 105})  ; => true
(m/validate [:set int?] #{:a :b})    ; => false
```

#### Vectors
```clojure
(m/validate [:vector int?] [1 2 3])     ; => true
(m/validate [:vector int?] (list 1 2 3)) ; => false
```

#### Sequences
```clojure
(m/validate [:sequential any?] (list "this" 'is :number 42)) ; => true
(m/validate [:sequential int?] [42 105])                     ; => true
```

#### Tuples
```clojure
(m/validate [:tuple keyword? string? number?] [:bing "bang" 42]) ; => true
```

### Advanced Schema Types

#### Maybe Schemas
```clojure
(m/validate [:maybe string?] "bingo") ; => true
(m/validate [:maybe string?] nil)     ; => true
(m/validate [:maybe string?] :bingo)  ; => false
```

#### Function Schemas
```clojure
(def my-schema
  [:and
   [:map
    [:x int?]
    [:y int?]]
   [:fn (fn [{:keys [x y]}] (> x y))]])

(m/validate my-schema {:x 2, :y 1}) ; => true
(m/validate my-schema {:x 1, :y 2}) ; => false
```

### Error Handling and Messages

#### Basic Error Explanation
```clojure
(require '[malli.error :as me])

(-> [:map
     [:id int?]
     [:name string?]]
    (m/explain {:id "1", :name 2})
    (me/humanize))
```

#### Custom Error Messages
```clojure
(-> [:map
     [:id int?]
     [:size [:enum {:error/message "should be: S|M|L"}
             "S" "M" "L"]]
     [:age [:fn {:error/fn (fn [{:keys [value]} _] (str value ", should be > 18"))}
            (fn [x] (and (int? x) (> x 18)))]]]
    (m/explain {:size "XL", :age 10})
    (me/humanize))
```

### Transformations

#### String Transformation
```clojure
(require '[malli.transform :as mt])

(m/decode int? "42" mt/string-transformer)         ; => 42
(m/encode int? 42 mt/string-transformer)          ; => "42"

;; Optimized decoder
(def decode (m/decoder int? mt/string-transformer))
(decode "42")                                     ; => 42
```

#### JSON Transformation
```clojure
(m/decode
  Address
  {:street "Street 1", :country "FI"}
  mt/json-transformer)

;; With validation
(m/coerce Address
          {:street "Street 1", :country "FI"}
          mt/json-transformer)
```

#### Default Values
```clojure
(m/decode [:and {:default 42} int?] nil mt/default-value-transformer)
; => 42

;; With custom defaults
(m/decode
  [:map
   [:name [:string {:default "anonymous"}]]
   [:age [:int {:default 18}]]]
  {}
  mt/default-value-transformer)
```

## Function Schemas and Validation

### Basic Function Schemas
```clojure
;; Basic function definitions with schemas
(def =>plus [:=> [:cat :int :int] :int])    ; Two ints -> int
(def =>greet [:=> [:cat :string] :string])  ; String -> string

;; Function predicate schemas
(m/validate fn? plus)      ; => true
(m/validate ifn? plus)     ; => true
```

### Advanced Function Schemas

#### Multi-arity Functions
```clojure
(def =>my-fn
  (m/schema
    [:function {:registry {::small-int [:int {:min -100, :max 100}]}}
     [:=> [:cat ::small-int] :int]
     [:=> [:cat ::small-int ::small-int [:* ::small-int]] :int]]
    {::m/function-checker mg/function-checker}))
```

#### Function Guards
```clojure
;; Function with guard ensuring arg < return value
(def arg<ret
  (m/schema
   [:=>
    [:cat :int]
    :int
    [:fn {:error/message "argument should be less than return"}
     (fn [[[arg] ret]] (< arg ret))]]
   {::m/function-checker mg/function-checker}))
```

### Function Instrumentation

#### Basic Instrumentation
```clojure
(def pow
  (m/-instrument
    {:schema [:=> [:cat :int] [:int {:max 6}]]}
    (fn [x] (* x x))))

;; Usage and validation
(pow 2)    ; => 4
(pow "2")  ; Throws error - invalid input
(pow 4)    ; Throws error - output > 6
```

#### Development Mode Instrumentation
```clojure
(require '[malli.dev :as dev])
(require '[malli.dev.pretty :as pretty])

;; Start development mode with pretty errors
(dev/start! {:report (pretty/reporter)})

;; Define and annotate function
(defn plus1 [x] (inc x))
(m/=> plus1 [:=> [:cat :int] [:int {:max 6}]])
```

### Function Definition Methods

#### Method 1: Schema Annotation
```clojure
(defn plus1 [x] (inc x))
(m/=> plus1 [:=> [:cat :int] small-int])
```

#### Method 2: Function Metadata
```clojure
(defn minus
  "A function with schema in metadata"
  {:malli/schema [:=> [:cat :int] small-int]}
  [x]
  (dec x))
```

#### Method 3: Inline Schema Definition
```clojure
(require '[malli.experimental :as mx])

(mx/defn times :- :int
  "Multiply x and y"
  [x :- :int, y :- small-int]
  (* x y))
```

### Function Schema Examples

#### Simple Validation
```clojure
(def schema [:=> [:cat :int :int] :int])
(def plus (fn [x y] (+ x y)))

(m/validate schema plus)                    ; => true
(m/validate schema str)                     ; => false
```

#### Complex Schemas
```clojure
;; Function returning smaller int than input
(def decrementing-schema
  [:=>
   [:cat :int]
   :int
   [:fn (fn [[[input] output]] (> input output))]])

;; Multi-arity function schema
(def multi-arity-schema
  [:function
   [:=> [:cat :int] :int]
   [:=> [:cat :int :int] :int]])
```

### Function Generation and Testing
```clojure
(def fn-schema [:=> [:cat :int] [:int {:max 6}]])
(def generated-fn (mg/generate fn-schema))

(generated-fn 1)          ; Returns valid int ≤ 6
(generated-fn "invalid")  ; Throws validation error
```

### Schema Registry

#### Custom Registry
```clojure
(def registry
  (merge
    (m/class-schemas)
    (m/comparator-schemas)
    (m/base-schemas)
    {:neg-int (m/-simple-schema {:type :neg-int, :pred neg-int?})
     :pos-int (m/-simple-schema {:type :pos-int, :pred pos-int?})}))

;; Usage with custom registry
(m/validate [:or :pos-int :neg-int] 123 {:registry registry})
```

#### Local Registry
```clojure
(def Adult
  [:map {:registry {::age [:and int? [:> 18]]}}
   [:age ::age]])
```

### Schema Generation and Testing
```clojure
(require '[malli.generator :as mg])

;; Generate sample data
(mg/generate [:map
              [:id string?]
              [:age [:and int? [:> 18]]]
              [:tags [:set keyword?]]])

;; With seed for reproducibility
(mg/generate [:enum "a" "b" "c"] {:seed 42})

;; Sample multiple values
(mg/sample [:and int? [:> 10] [:< 100]] {:seed 123})
```

### Schema Inference
```clojure
(require '[malli.provider :as mp])

(def samples
  [{:id "1", :name "Alice", :age 30}
   {:id "2", :name "Bob", :age 25}])

(mp/provide samples)
;; Returns inferred schema based on samples
```

### Visualization

#### DOT Language Export
```clojure
(require '[malli.dot :as md])

(md/transform Address)
;; Returns DOT language representation
```

#### PlantUML Export
```clojure
(require '[malli.plantuml :as plantuml])

(plantuml/transform Address)
;; Returns PlantUML representation
```

## Schema Types Reference

### Basic Types
- `:any` - Any value
- `:string` - String values
- `:int` - Integer values
- `:double` - Double/float values
- `:boolean` - Boolean values
- `:keyword` - Keyword values
- `:uuid` - UUID values
- `:uri` - URI values

### Collection Types
- `:map` - Object/map schemas
- `:vector` - Vector schemas
- `:set` - Set schemas
- `:tuple` - Fixed-length heterogeneous vectors
- `:sequential` - Any sequential collection
- `:seqable` - Any seqable collection

### Logic Types
- `:and` - All conditions must match
- `:or` - Any condition must match
- `:not` - Negation of condition
- `:maybe` - Value or nil

### Special Types
- `:enum` - Enumerated values
- `:multi` - Multi-method like dispatch
- `:ref` - Reference to another schema
- `:=>` - Function input/output schema

## Common Properties

### General Properties
- `:title` - Schema title
- `:description` - Schema description
- `:optional` - For optional keys
- `:default` - Default value

### Validation Properties
- `:min` - Minimum value/length
- `:max` - Maximum value/length
- `:error/message` - Custom error message
- `:error/fn` - Custom error function

### Generation Properties
- `:gen/fmap` - Transform generated values
- `:gen/elements` - Specify elements to choose from
- `:gen/schema` - Override generator schema

### Transformation Properties
- `:decode` - Custom decode function
- `:encode` - Custom encode function
- `:json-schema` - JSON Schema overrides

## Performance Tips

1. Use `m/validator` for repeated validations
2. Use `m/decoder`/`m/encoder` for repeated transformations
3. Prefer schema types (`:string`) over predicates (`string?`) when using properties
4. Use closed maps when possible to catch typos
5. Consider using local registries for recursive schemas

## Examples
## Value transformation

```clojure
(require '[malli.transform :as mt])
```

Two-way schema-driven value transformations with `m/decode` and `m/encode` using a `Transformer` instance.

Default Transformers include:

| name                              | description                                         |
|:----------------------------------|-----------------------------------------------------|
| `mt/string-transformer`           | transform between strings and EDN                   |
| `mt/json-transformer`             | transform between JSON and EDN                      |
| `mt/strip-extra-keys-transformer` | drop extra keys from maps                           |
| `mt/default-value-transformer`    | applies default values from schema properties       |
| `mt/key-transformer`              | transforms map keys                                |
| `mt/collection-transformer`       | conversion between collections (e.g. set -> vector) |

**NOTE**: the included transformers are best-effort, i.e. they won't throw on bad input, they will just pass the input value through unchanged. You should make sure your schema validation catches these non-transformed values. Custom transformers should follow the same idiom.

Simple usage:

```clojure
(m/decode int? "42" mt/string-transformer)
; 42

(m/encode int? 42 mt/string-transformer)
; "42"
```

For performance, precompute the transformations with `m/decoder` and `m/encoder`:

```clojure
(def decode (m/decoder int? mt/string-transformer))

(decode "42")
; 42

(def encode (m/encoder int? mt/string-transformer))

(encode 42)
; "42"
```

### Coercion

For both decoding + validating the results (throwing exception on error), there is `m/coerce` and `m/coercer`:

```clojure
(m/coerce :int "42" mt/string-transformer)
; 42

((m/coercer :int mt/string-transformer) "42")
; 42

(m/coerce :int "invalid" mt/string-transformer)
; =throws=> :malli.core/invalid-input {:value "invalid", :schema :int, :explain {:schema :int, :value "invalid", :errors ({:path [], :in [], :schema :int, :value "invalid"})}}
```

Coercion can be applied without transformer, doing just validation:

```clojure
(m/coerce :int 42)
; 42

(m/coerce :int "42")
; =throws=> :malli.core/invalid-input {:value "42", :schema :int, :explain {:schema :int, :value "42", :errors ({:path [], :in [], :schema :int, :value "42"})}}
```

Exception-free coercion with continuation-passing style:

```clojure
(m/coerce :int "fail" nil (partial prn "success:") (partial prn "error:"))
; =prints=> "error:" {:value "fail", :schema :int, :explain ...}
```

### Advanced Transformations

Transformations are recursive:

```clojure
(m/decode
  Address
  {:id "Lillan",
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  mt/json-transformer)
;{:id "Lillan",
; :tags #{:coffee :artesan :garden},
; :address {:street "Ahlmanintie 29"
;           :city "Tampere"
;           :zip 33100
;           :lonlat [61.4858322 23.7854658]}}
```

Transform map keys:

```clojure
(m/encode
  Address
  {:id "Lillan",
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  (mt/key-transformer {:encode name}))
;{"id" "Lillan",
; "tags" ["coffee" "artesan" "garden"],
; "address" {"street" "Ahlmanintie 29"
;            "city" "Tampere"
;            "zip" 33100
;            "lonlat" [61.4858322 23.7854658]}}
```

Transforming homogenous `:enum` or `:=`s (supports automatic type detection of `:keyword`, `:symbol`, `:int` and `:double`):

```clojure
(m/decode [:enum :kikka :kukka] "kukka" mt/string-transformer)
; => :kukka
```

Transformers can be composed with `mt/transformer`:

```clojure
(def strict-json-transformer
  (mt/transformer
    mt/strip-extra-keys-transformer
    mt/json-transformer))

(m/decode
  Address
  {:id "Lillan",
   :EVIL "LYN"
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :DARK "ORKO"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  strict-json-transformer)
;{:id "Lillan",
; :tags #{:coffee :artesan :garden},
; :address {:street "Ahlmanintie 29"
;           :city "Tampere"
;           :zip 33100
;           :lonlat [61.4858322 23.7854658]}}
```

Schema properties can be used to override default transformations:

```clojure
(m/decode
  [string? {:decode/string clojure.string/upper-case}]
  "kerran" mt/string-transformer)
; => "KERRAN"
```

This works too:

```clojure
(m/decode
  [string? {:decode {:string clojure.string/upper-case}}]
  "kerran" mt/string-transformer)
; => "KERRAN"
```

Decoders and encoders as interceptors (with `:enter` and `:leave` stages):

```clojure
(m/decode
  [string? {:decode/string {:enter clojure.string/upper-case}}]
  "kerran" mt/string-transformer)
; => "KERRAN"
```

```clojure
(m/decode
  [string? {:decode/string {:enter #(str "olipa_" %)
                            :leave #(str % "_avaruus")}}]
  "kerran" mt/string-transformer)
; => "olipa_kerran_avaruus"
```

To access Schema (and options) use `:compile`:

```clojure
(m/decode
  [int? {:math/multiplier 10
         :decode/math {:compile (fn [schema _]
                                  (let [multiplier (:math/multiplier (m/properties schema))]
                                    (fn [x] (* x multiplier))))}}]
  12
  (mt/transformer {:name :math}))
; => 120
```

## To and from JSON

The `m/encode` and `m/decode` functions work on clojure data. To go
from clojure data to JSON, you need a JSON library like
[jsonista](https://github.com/metosin/jsonista). Additionally, since
`m/decode` doesn't check the schema, you need to run `m/validate` (or
`m/explain`) if you want to make sure your data conforms to your
schema.

To JSON:

```clojure
(def Tags
  (m/schema [:map
             {:closed true}
             [:tags [:set :keyword]]]))
(jsonista.core/write-value-as-string
 (m/encode Tags
           {:tags #{:bar :quux}}
           mt/json-transformer))
; => "{\"tags\":[\"bar\",\"quux\"]}"
```

From JSON without validation:

```clojure
(m/decode Tags
          (jsonista.core/read-value "{\"tags\":[\"bar\",[\"quux\"]]}"
                                    jsonista.core/keyword-keys-object-mapper)
          mt/json-transformer)
; => {:tags #{:bar ["quux"]}}
```

From JSON with validation:

```clojure
(m/explain Tags
           (m/decode Tags
                     (jsonista.core/read-value "{\"tags\":[\"bar\",[\"quux\"]]}"
                                               jsonista.core/keyword-keys-object-mapper)
                     mt/json-transformer))
; => {:schema [:map {:closed true} [:tags [:set :keyword]]],
;     :value {:tags #{:bar ["quux"]}},
;     :errors ({:path [:tags 0], :in [:tags ["quux"]], :schema :keyword, :value ["quux"]})}
```

```clojure
(m/validate Tags
            (m/decode Tags
                      (jsonista.core/read-value "{\"tags\":[\"bar\",\"quux\"]}" ; <- note! no error
                                                jsonista.core/keyword-keys-object-mapper)
                      mt/json-transformer))
; => true
```

For performance, it's best to prebuild the validator, decoder and explainer:

```clojure
(def validate-Tags (m/validator Tags))
(def decode-Tags (m/decoder Tags mt/json-transformer))
(-> (jsonista.core/read-value "{\"tags\":[\"bar\",\"quux\"]}"
                              jsonista.core/keyword-keys-object-mapper)
    decode-Tags
    validate-Tags)
; => true
```

## Default values

Applying default values:

```clojure
(m/decode [:and {:default 42} int?] nil mt/default-value-transformer)
; => 42
```

With custom key and type defaults:

```clojure
(m/decode
  [:map
   [:user [:map
           [:name :string]
           [:description {:ui/default "-"} :string]]]]
  nil
  (mt/default-value-transformer
    {:key :ui/default
     :defaults {:map (constantly {})
                :string (constantly "")}}))
; => {:user {:name "", :description "-"}}
```

With custom function:

```clojure
(m/decode
 [:map
  [:os [:string {:property "os.name"}]]
  [:timezone [:string {:property "user.timezone"}]]]
 {}
 (mt/default-value-transformer
  {:key :property
   :default-fn (fn [_ x] (System/getProperty x))}))
; => {:os "Mac OS X", :timezone "Europe/Helsinki"}
```

Optional Keys are not added by default:

```clojure
(m/decode
 [:map
  [:name [:string {:default "kikka"}]]
  [:description {:optional true} [:string {:default "kikka"}]]]
 {}
 (mt/default-value-transformer))
; => {:name "kikka"}
```

Adding optional keys too via `::mt/add-optional-keys` option:

```clojure
(m/decode
 [:map
  [:name [:string {:default "kikka"}]]
  [:description {:optional true} [:string {:default "kikka"}]]]
 {}
 (mt/default-value-transformer {::mt/add-optional-keys true}))
; => {:name "kikka", :description "kikka"}
```

Single sweep of defaults & string encoding:

```clojure
(m/encode
  [:map {:default {}}
   [:a [int? {:default 1}]]
   [:b [:vector {:default [1 2 3]} int?]]
   [:c [:map {:default {}}
        [:x [int? {:default 42}]]
        [:y int?]]]
   [:d [:map
        [:x [int? {:default 42}]]
        [:y int?]]]
   [:e int?]]
  nil
  (mt/transformer
    mt/default-value-transformer
    mt/string-transformer))
;{:a "1"
; :b ["1" "2" "3"]
; :c {:x "42"}}
```

## Programming with schemas

```clojure
(require '[malli.util :as mu])
```

Updating Schema properties:

```clojure
(mu/update-properties [:vector int?] assoc :min 1)
; => [:vector {:min 1} int?]
```

Lifted `clojure.core` function to work with schemas: `select-keys`, `dissoc`, `get`, `assoc`, `update`, `get-in`, `assoc-in`, `update-in`

```clojure
(mu/get-in Address [:address :lonlat])
; => [:tuple double? double?]

(mu/update-in Address [:address] mu/assoc :country [:enum "fi" "po"])
;[:map
; [:id string?]
; [:tags [:set keyword?]]
; [:address
;  [:map [:street string?]
;   [:city string?]
;   [:zip int?]
;   [:lonlat [:tuple double? double?]]
;   [:country [:enum "fi" "po"]]]]]

(-> Address
    (mu/dissoc :address)
    (mu/update-properties assoc :title "Address"))
;[:map {:title "Address"}
; [:id string?]
; [:tags [:set keyword?]]]
```

Making keys optional or required:

```clojure
(mu/optional-keys [:map [:x int?] [:y int?]])
;[:map
; [:x {:optional true} int?]
; [:y {:optional true} int?]]

(mu/optional-keys [:map [:x int?] [:y int?]]
                  [:x])
;[:map
; [:x {:optional true} int?]
; [:y int?]]

(mu/required-keys [:map [:x {:optional true} int?] [:y {:optional true} int?]])
;[:map
; [:x int?]
; [:y int?]]

(mu/required-keys [:map [:x {:optional true} int?] [:y {:optional true} int?]]
                  [:x])
;[:map
; [:x int?]
; [:y {:optional true} int?]]
```

Closing and opening all `:map` schemas recursively:

```clojure
(def abcd
  [:map {:title "abcd"}
   [:a int?]
   [:b {:optional true} int?]
   [:c [:map
        [:d int?]]]])

(mu/closed-schema abcd)
;[:map {:title "abcd", :closed true}
; [:a int?]
; [:b {:optional true} int?]
; [:c [:map {:closed true}
;      [:d int?]]]]

(-> abcd
    mu/closed-schema
    mu/open-schema)
;[:map {:title "abcd"}
; [:a int?]
; [:b {:optional true} int?]
; [:c [:map
;      [:d int?]]]]
```

Merging Schemas (last value wins):

```clojure
(mu/merge
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  [:map
   [:description {:optional true} string?]
   [:address
    [:map
     [:country string?]]]])
;[:map
; [:name string?]
; [:description {:optional true} string?]
; [:address [:map
;            [:street string?]
;            [:country string?]]]]
```

With `:and`, first child is used in merge:

```clojure
(mu/merge
  [:and {:type "entity"}
   [:map {:title "user"}
    [:name :string]]
   map?]
  [:map {:description "aged"} [:age :int]])
;[:and {:type "entity"}
; [:map {:title "user", :description "aged"}
;  [:name :string]
;  [:age :int]]
; map?]
```

Schema unions (merged values of both schemas are valid for union schema):

```clojure
(mu/union
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  [:map
   [:description {:optional true} string?]
   [:address
    [:map
     [:country string?]]]])
;[:map
; [:name string?]
; [:description {:optional true} string?]
; [:address [:map
;            [:street string?]
;            [:country [:or [:enum "finland" "poland"] string?]]]]]
```

Adding generated example values to Schemas:

```clojure
(m/walk
  [:map
   [:name string?]
   [:description string?]
   [:address
    [:map
     [:street string?]
     [:country [:enum "finland" "poland"]]]]]
  (m/schema-walker
    (fn [schema]
      (mu/update-properties schema assoc :examples (mg/sample schema {:size 2, :seed 20})))))
;[:map
; {:examples ({:name "", :description "", :address {:street "", :country "poland"}}
;             {:name "W", :description "x", :address {:street "8", :country "finland"}})}
; [:name [string? {:examples ("" "")}]]
; [:description [string? {:examples ("" "")}]]
; [:address
;  [:map
;   {:examples ({:street "", :country "finland"} {:street "W", :country "poland"})}
;   [:street [string? {:examples ("" "")}]]
;   [:country [:enum {:examples ("finland" "poland")} "finland" "poland"]]]]]
```

Finding first value (prewalk):

```clojure
(mu/find-first
  [:map
   [:x int?]
   [:y [:vector [:tuple
                 [:or [:and {:salaisuus "turvassa"} boolean?] int?]
                 [:schema {:salaisuus "vaarassa"} false?]]]]
   [:z [:string {:salaisuus "piilossa"}]]]
  (fn [schema _ _]
    (-> schema m/properties :salaisuus)))
; => "turvassa"
```

## Declarative schema transformation

There are also declarative versions of schema transforming utilities in `malli.util/schemas`. These include `:merge`, `:union` and `:select-keys`:

```clojure
(def registry (merge (m/default-schemas) (mu/schemas)))

(def Merged
  (m/schema
    [:merge
     [:map [:x :string]]
     [:map [:y :int]]]
    {:registry registry}))

Merged
;[:merge
; [:map [:x :string]]
; [:map [:y :int]]]

(m/deref Merged)
;[:map
; [:x :string]
; [:y :int]]

(m/validate Merged {:x "kikka", :y 6})
; => true
```

`:union` is similar to `:or`, except `:union` combines map schemas in different disjuncts with `:or`.
For example, `UnionMaps` is equivalent to `[:map [:x [:or :int :string]] [:y [:or :int :string]]]`.

```clojure
(def OrMaps
  (m/schema
    [:or
     [:map [:x :int] [:y :string]]
     [:map [:x :string] [:y :int]]]
    {:registry registry}))

(def UnionMaps
  (m/schema
    [:union
     [:map [:x :int] [:y :string]]
     [:map [:x :string] [:y :int]]]
    {:registry registry}))

(m/validate OrMaps {:x "kikka" :y "kikka"})
; => false

(m/validate UnionMaps {:x "kikka" :y "kikka"})
; => true
```

`:merge` and `:union` differ on schemas with common keys. `:merge` chooses the right-most
schema of common keys, and `:union` combines them with `:or`.
For example, `MergedCommon` is equivalent to `[:map [:x :int]]`, and `UnionCommon`
is equivalent to `[:map [:x [:or :string :int]]]`.

```clojure
(def MergedCommon
  (m/schema
    [:merge
     [:map [:x :string]]
     [:map [:x :int]]]
    {:registry registry}))

(def UnionCommon
  (m/schema
    [:union
     [:map [:x :string]]
     [:map [:x :int]]]
    {:registry registry}))

(m/validate MergedCommon {:x "kikka"})
; => true
(m/validate MergedCommon {:x 1})
; => false
(m/validate UnionCommon {:x "kikka"})
; => true
(m/validate UnionCommon {:x 1})
; => true
```

### Distributive schemas

`:merge` also distributes over `:multi` in a [similar way](https://en.wikipedia.org/wiki/Distributive_property) to how multiplication
distributes over addition in arithmetic. There are two transformation rules, applied in the following order:

```clojure
;; right-distributive
[:merge [:multi M1 M2 ...] M3]
=>
[:multi [:merge M1 M3] [:merge M2 M3] ...]

;; left-distributive
[:merge M1 [:multi M2 M3 ...]]
=>
[:multi [:merge M1 M2] [:merge M1 M3] ...]
```

For `:merge` with more than two arguments, the rules are applied iteratively left-to-right
as if the following transformation was applied:

```clojure
[:merge M1 M2 M3 M4 ...]
=>
[:merge
 [:merge
  [:merge M1 M2]
  M3]
 M4]
...
```

The distributive property of `:multi` is useful combined with `:merge`
if you want all clauses of a `:multi` to share extra entries.

Here are concrete examples of applying the rules:

```clojure
;; left-distributive
(m/deref
 [:merge
  [:map [:x :int]]
  [:multi {:dispatch :y}
   [1 [:map [:y [:= 1]]]]
   [2 [:map [:y [:= 2]]]]]]
 {:registry registry})
; => [:multi {:dispatch :y}
;     [1 [:map [:x :int] [:y [:= 1]]]]
;     [2 [:map [:x :int] [:y [:= 2]]]]]

;; right-distributive
(m/deref
 [:merge
  [:multi {:dispatch :y}
   [1 [:map [:y [:= 1]]]]
   [2 [:map [:y [:= 2]]]]]
  [:map [:x :int]]]
 {:registry registry})
; => [:multi {:dispatch :y}
;     [1 [:map [:y [:= 1]] [:x :int]]]
;     [2 [:map [:y [:= 2]] [:x :int]]]]
```

It is not recommended to use local registries in schemas that are transformed.
Also be aware that merging non-maps via the distributive property inherits
the same semantics as `:merge`, which is based on [meta-merge](https://github.com/weavejester/meta-merge).

## Persisting schemas

Writing and Reading schemas as [EDN](https://github.com/edn-format/edn), no `eval` needed.

Following example requires [SCI](https://github.com/babashka/sci) or
[cherry](https://github.com/squint-cljs/cherry) as external dependency because
it includes a (quoted) function definition. See [Serializable
functions](#serializable-functions).

```clojure
(require '[malli.edn :as edn])

(-> [:and
     [:map
      [:x int?]
      [:y int?]]
     [:fn '(fn [{:keys [x y]}] (> x y))]]
    (edn/write-string)
    (doto prn) ; => "[:and [:map [:x int?] [:y int?]] [:fn (fn [{:keys [x y]}] (> x y))]]"
    (edn/read-string)
    (doto (-> (m/validate {:x 0, :y 1}) prn)) ; => false
    (doto (-> (m/validate {:x 2, :y 1}) prn))) ; => true
;[:and
; [:map
;  [:x int?]
;  [:y int?]]
; [:fn (fn [{:keys [x y]}] (> x y))]]
```

## Multi schemas

Closed dispatch with `:multi` schema and `:dispatch` property:

```clojure
(m/validate
  [:multi {:dispatch :type}
   [:sized [:map [:type keyword?] [:size int?]]]
   [:human [:map [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]]]
  {:type :sized, :size 10})
; true
```

Default branch with `::m/default`:

```clojure
(def valid?
  (m/validator
    [:multi {:dispatch :type}
     ["object" [:map-of :keyword :string]]
     [::m/default :string]]))

(valid? {:type "object", :key "1", :value "100"})
; => true

(valid? "SUCCESS!")
; => true

(valid? :failure)
; => false
```

Any function can be used for `:dispatch`:

```clojure
(m/validate
  [:multi {:dispatch first}
   [:sized [:tuple keyword? [:map [:size int?]]]]
   [:human [:tuple keyword? [:map [:name string?] [:address [:map [:country keyword?]]]]]]]
  [:human {:name "seppo", :address {:country :sweden}}])
; true
```

`:dispatch` values should be decoded before actual values:

```clojure
(m/decode
  [:multi {:dispatch :type
           :decode/string #(update % :type keyword)}
   [:sized [:map [:type [:= :sized]] [:size int?]]]
   [:human [:map [:type [:= :human]] [:name string?] [:address [:map [:country keyword?]]]]]]
  {:type "human"
   :name "Tiina"
   :age "98"
   :address {:country "finland"
             :street "this is an extra key"}}
  (mt/transformer mt/strip-extra-keys-transformer mt/string-transformer))
;{:type :human
; :name "Tiina"
; :address {:country :finland}}
```
