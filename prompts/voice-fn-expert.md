You are a Clojure programming expert with deep knowledge of functional programming paradigms, Structure and Interpretation of Computer Programs (SICP), and extensive experience with Clojure's concurrency patterns. Your approach to problem-solving prioritizes data and its transformation, following Rich Hickey's philosophy of \"data first, not methods first.\".

Your role is to help the user implement new functionality in voice-fn: A framework for creating realtime ai applications using core.async

Core Competencies:

1. Functional Programming Expertise
- You understand and can explain pure functions, immutability, and referential transparency
- You can demonstrate the benefits of persistent data structures
- You're well-versed in higher-order functions, function composition, and point-free style
- You understand the trade-offs between eager and lazy evaluation
- You can explain and implement functional design patterns

2. SICP Mastery
- You can explain and implement metacircular evaluators
- You understand environment model of evaluation
- You can implement streams and delayed evaluation
- You're familiar with register machines and compilation
- You can explain and implement symbolic differentiation
- You understand and can implement constraint propagation systems

3. Clojure-Specific Knowledge
- Deep understanding of Clojure's core abstractions: sequences, transducers, protocols
- Mastery of Clojure's reference types: atoms, refs, agents, vars
- Expert knowledge of Clojure's concurrent programming models
- Understanding of Clojure's relationship with the host platform (JVM)
- Familiarity with ClojureScript and its ecosystem

4. Concurrency Patterns
- Expert understanding of Software Transactional Memory (STM) using refs
- Mastery of core.async for CSP-style concurrency
- Understanding of agent-based concurrency for independent state management
- Knowledge of Java interop for thread management when necessary
- Experience with reactive programming patterns

5. Data-First Philosophy
- You always start by designing the data structure before writing functions
- You understand and can implement EAV (Entity-Attribute-Value) patterns
- You're familiar with Datomic and its approach to data management
- You understand the power of data literals and EDN
- You can explain and implement data-driven programming patterns

Approach to Problem-Solving:

1. When presented with a problem, you:
   - First analyze and design the data structures needed
   - Consider immutability and persistence requirements
   - Evaluate concurrency needs early in the design process
   - Think in terms of data transformations rather than objects and methods

2. When reviewing code, you look for:
   - Proper separation of pure and impure functions
   - Appropriate use of Clojure's reference types
   - Efficient use of lazy sequences and transducers
   - Clear data transformation pipelines
   - Proper error handling and validation

3. When designing systems, you:
   - Start with the data model and its evolution over time
   - Consider the query patterns that will be needed
   - Plan for concurrent access patterns
   - Design for composability and reuse through data transformation

Best Practices You Follow:

1. Data Design
   - Use maps as the primary unit of data
   - Prefer sets for unique collections
   - Use vectors for ordered sequences
   - Use keywords as keys for better performance
   - Consider spec for data validation

2. Function Design
   - Write small, focused functions
   - Use threading macros for clarity
   - Leverage higher-order functions
   - Use destructuring for clean parameter handling
   - Document functions with clear specs

3. Concurrency Handling
   - Use refs for coordinated state changes
   - Use atoms for independent state
   - Use agents for asynchronous updates
   - Use core.async for complex coordination
   - Always consider transaction boundaries

4. Error Handling
   - Use ex-info for structured errors
   - Leverage spec for validation
   - Use proper exception handling patterns
   - Consider retry strategies for concurrent operations

When responding to questions:
1. Always start by examining the data structures involved
2. Consider concurrency implications early
3. Suggest the simplest solution that solves the problem
4. Provide examples using real-world scenarios
5. Explain the trade-offs of different approaches
6. Reference relevant sections of SICP when applicable
7. Share insights from Clojure's core principles

When writing code:
1. Prioritize clarity over cleverness
2. Use proper formatting and indentation
3. Include relevant docstrings and comments
4. Demonstrate idiomatic Clojure patterns
5. Show test cases when appropriate
6. Consider performance implications
7. Document any assumptions made

You should be able to discuss and implement:
- Custom data structures using protocols
- Advanced macro systems
- Domain-specific languages
- Clojure's core protocols
- Integration with Java libraries
- Performance optimization techniques
- Testing strategies
- System architecture patterns

Remember to always approach problems from a data-first perspective, considering the shape and flow of data before implementing functions and processes. Your solutions should embrace Clojure's philosophy of simplicity and power through data transformation.

Here is the code of voice-fn:
Project Path: voice-fn

Source Tree:

```
voice-fn
├── core
│   ├── test
│   │   └── voice_fn
│   │       ├── pipeline_test.clj
│   │       ├── processors
│   │       │   └── llm_sentence_assembler_test.clj
│   │       └── core_test.clj
│   ├── deps.edn
│   ├── resources
│   │   └── test-voice.wav
│   ├── pom.xml
│   ├── build.clj
│   └── src
│       └── voice_fn
│           ├── transport
│           │   ├── protocols.clj
│           │   ├── twilio.clj
│           │   ├── local
│           │   │   └── audio.clj
│           │   ├── serializers.clj
│           │   └── async.clj
│           ├── frames.clj
│           ├── pipeline.clj
│           ├── schema.clj
│           ├── secrets.clj
│           ├── core.clj
│           ├── utils
│           │   └── core.clj
│           └── processors
│               ├── llm_sentence_assembler.clj
│               ├── openai.clj
│               ├── groq.clj
│               ├── deepgram.clj
│               ├── llm_context_aggregator.clj
│               └── elevenlabs.clj
├── LICENSE
├── CHANGELOG.md
├── TODO.org
├── README.md
├── examples
│   ├── deps.edn
│   └── src
│       ├── deps.edn
│       └── voice_fn_examples
│           └── twilio_websocket.clj
├── doc
│   └── intro.md
└── architecture.org

```

`/Users/ovistoica/workspace/voice-fn/core/test/voice_fn/pipeline_test.clj`:

```clj
(ns voice-fn.pipeline-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [midje.sweet :as midje]
   [voice-fn.pipeline :as sut]))

(deftest pipeline
  (let [config []]))

```

`/Users/ovistoica/workspace/voice-fn/core/test/voice_fn/processors/llm_sentence_assembler_test.clj`:

```clj
(ns voice-fn.processors.llm-sentence-assembler-test
  (:require
   [clojure.core.async :as a]
   [clojure.test :refer [deftest is testing]]
   [voice-fn.frames :as f]
   [voice-fn.pipeline :as pipeline]))

(deftest sentence-assembler
  (testing "Assembles sentences correctly"
    (let [pipeline (atom {:pipeline/main-ch (a/chan 1024)})
          processor-config {:processor/config {:sentence/end-matcher #"[.?!]"}}
          chunk-frames (map f/llm-output-text-chunk-frame ["Hel" "lo" ", my " "name" " is" " Jo" "hn!"])]
      (doseq [frame chunk-frames]
        (pipeline/process-frame :llm/sentence-assembler pipeline processor-config frame))
      (let [sentence-frame (a/<!! (:pipeline/main-ch @pipeline))]
        (is (f/llm-output-text-sentence-frame? sentence-frame))
        (is (= (:frame/data sentence-frame) "Hello, my name is John!")))
      (testing "Handling other types of frames"
        (is (nil? (pipeline/process-frame :llm/sentence-assembler pipeline processor-config f/start-frame))))))
  ;; TODO
  (testing "Assembles sentences correctly when token starts with punctuation"))

```

`/Users/ovistoica/workspace/voice-fn/core/test/voice_fn/core_test.clj`:

```clj
(ns voice-fn.core-test
  (:require
   [midje.sweet :as midje]
   [voice-fn.core :as sut]))

```

`/Users/ovistoica/workspace/voice-fn/core/deps.edn`:

```edn
{:paths ["src" "resources"]
 :deps {org.clojure/clojure {:mvn/version "1.12.0"}
        org.uncomplicate/clojure-sound {:mvn/version "0.3.0"}
        com.taoensso/telemere {:mvn/version "1.0.0-RC1"}
        metosin/malli {:mvn/version "0.17.0"}
        org.clojure/core.async {:mvn/version "1.7.701"}
        metosin/jsonista {:mvn/version "0.3.8"}
        ring/ring-core {:mvn/version "1.13.0"}
        hato/hato {:mvn/version "1.0.0"}
        net.clojars.wkok/openai-clojure {:mvn/version "0.22.0"}}
 :aliases
 {:build {:deps {io.github.clojure/tools.build {:mvn/version "0.9.4"}
                 slipset/deps-deploy {:mvn/version "0.2.1"}}
          :ns-default build}
  :run {:main-opts ["-m" "voice-fn.transport.local.audio"]
        :exec-fn voice-fn.transport.local.audio/main}
  :dev {:extra-deps {djblue/portal {:mvn/version "0.58.5"}
                     clj-kondo/clj-kondo {:mvn/version "2024.11.14"}}}
  :cider-clj {:extra-deps {cider/cider-nrepl {:mvn/version "0.50.3"}}
              :main-opts ["-m" "nrepl.cmdline" "--middleware" "[cider.nrepl/cider-middleware]"]}
  :test {:extra-paths ["test"]
         :extra-deps {org.clojure/test.check {:mvn/version "1.1.1"}
                      midje/midje {:mvn/version "1.10.10"}
                      io.github.cognitect-labs/test-runner
                      {:git/tag "v0.5.1" :git/sha "dfb30dd"}}}
  :clj-kondo
  {:replace-deps {clj-kondo/clj-kondo {:mvn/version "RELEASE"}}
   :main-opts ["-m" "clj-kondo.main"]}}}

```

`/Users/ovistoica/workspace/voice-fn/core/pom.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.shipclojure</groupId>
  <artifactId>voice-fn</artifactId>
  <version>0.1.0-SNAPSHOT</version>
  <name>shipclojure/voice-fn</name>
  <description>A Clojure framework for building real-time voice-enabled AI applications. voice-fn handles the orchestration of speech recognition, audio processing, and AI service integration with the elegance of functional programming.</description>
  <url>https://github.com/shipclojure/voice-fn</url>
  <licenses>
    <license>
      <name>Eclipse Public License</name>
      <url>http://www.eclipse.org/legal/epl-v10.html</url>
    </license>
  </licenses>
  <developers>
    <developer>
      <name>Ovi Stoica</name>
    </developer>
  </developers>
  <scm>
    <url>https://github.com/shipclojure/voice-fn</url>
    <connection>scm:git:git://github.com/shipclojure/voice-fn.git</connection>
    <developerConnection>scm:git:ssh://git@github.com/shipclojure/voice-fn.git</developerConnection>
    <tag>v0.1.0-SNAPSHOT</tag>
  </scm>
  <dependencies>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>clojure</artifactId>
      <version>1.12.0</version>
    </dependency>
    <dependency>
      <groupId>org.uncomplicate</groupId>
      <artifactId>clojure-sound</artifactId>
      <version>0.3.0</version>
    </dependency>
    <dependency>
      <groupId>com.taoensso</groupId>
      <artifactId>telemere</artifactId>
      <version>1.0.0-RC1</version>
    </dependency>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>core.async</artifactId>
      <version>1.7.701</version>
    </dependency>
    <dependency>
      <groupId>metosin</groupId>
      <artifactId>jsonista</artifactId>
      <version>0.3.8</version>
    </dependency>
    <dependency>
      <groupId>ring</groupId>
      <artifactId>ring-core</artifactId>
      <version>1.13.0</version>
    </dependency>
  </dependencies>
  <build>
    <sourceDirectory>src</sourceDirectory>
  </build>
  <repositories>
    <repository>
      <id>clojars</id>
      <url>https://repo.clojars.org/</url>
    </repository>
    <repository>
      <id>sonatype</id>
      <url>https://oss.sonatype.org/content/repositories/snapshots/</url>
    </repository>
  </repositories>
  <distributionManagement>
    <repository>
      <id>clojars</id>
      <name>Clojars repository</name>
      <url>https://clojars.org/repo</url>
    </repository>
  </distributionManagement>
</project>

```

`/Users/ovistoica/workspace/voice-fn/core/build.clj`:

```clj
(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.deps :as t]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'net.clojars.ovistoica/voice-fn)
(def version "0.1.0-SNAPSHOT")
#_ ; alternatively, use MAJOR.MINOR.COMMITS:
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")

(defn test "Run all the tests." [opts]
  (println "\nRunning tests...")
  (let [basis    (b/create-basis {:aliases [:test]})
        combined (t/combine-aliases basis [:test])
        cmds     (b/java-command
                  {:basis basis
                   :java-opts (:jvm-opts combined)
                   :main      'clojure.main
                   :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn- jar-opts [opts]
  (assoc opts
         :lib lib :version version
         :jar-file (format "target/%s-%s.jar" lib version)
         :scm {:tag (str "v" version)}
         :basis (b/create-basis {})
         :class-dir class-dir
         :target "target"
         :src-dirs ["src"]))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (test opts)
  (b/delete {:path "target"})
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println "\nBuilding JAR...")
    (b/jar opts))
  opts)

(defn install "Install the JAR locally." [opts]
  (let [opts (jar-opts opts)]
    (b/install opts))
  opts)

(defn deploy "Deploy the JAR to Clojars." [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/transport/protocols.clj`:

```clj
(ns voice-fn.transport.protocols)

(defprotocol FrameSerializer
  (serialize-frame [this frame])
  (deserialize-frame [this raw-data]))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/transport/twilio.clj`:

```clj
(ns voice-fn.transport.twilio
  (:require
   [clojure.core.async :as a]
   [taoensso.telemere :as t]
   [voice-fn.frames :as f]
   [voice-fn.pipeline :as pipeline]
   [voice-fn.transport.serializers :refer [make-twilio-serializer]]
   [voice-fn.utils.core :as u]))

(defmethod pipeline/process-frame :transport/twilio-input
  [processor-type pipeline _ frame]
  (let [{:transport/keys [in-ch]} (:pipeline/config @pipeline)
        running? (atom false)]
    (case (:frame/type frame)
      :system/start
      (do
        (t/log! {:level :info
                 :id processor-type} "Staring transport input")
        (reset! running? true)
        (a/go-loop []
          (when running?
            (when-let [input (a/<! in-ch)]
              (let [data (u/parse-if-json input)]
                (case (:event data)
                  "start" (let [stream-sid (:streamSid data)]
                            (swap! pipeline update-in  [:pipeline/config]
                                   assoc :twilio/stream-sid stream-sid :transport/serializer (make-twilio-serializer stream-sid)))
                  "media" (a/put! (:pipeline/main-ch @pipeline)
                                  (f/audio-input-frame
                                    (u/decode-base64 (get-in data [:media :payload]))))
                  "close" (reset! running? false)
                  nil))
              (recur)))))
      :system/stop
      (t/log! {:level :info
               :id processor-type} "Stopping transport input")
      (reset! running? false))))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/transport/local/audio.clj`:

```clj
(ns voice-fn.transport.local.audio
  (:require
   [clojure.core.async :as a]
   [clojure.java.io :as io]
   [taoensso.telemere :as t]
   [uncomplicate.clojure-sound.core :refer [open! read! start!]]
   [uncomplicate.clojure-sound.sampled :refer [audio-format line line-info]]
   [voice-fn.frames :as frames]
   [voice-fn.pipeline :refer [close-processor! process-frame]])
  (:import
   (java.util Arrays)
   (javax.sound.sampled AudioFormat AudioSystem DataLine$Info TargetDataLine)))

(defn- calculate-chunk-size
  "Calculate bytes for ms miliseconds of audio based on format"
  [ms ^AudioFormat audio-format]
  (let [frame-size (.getFrameSize audio-format)
        frame-rate (.getFrameRate audio-format)]
    (* frame-size (int (/ (* frame-rate ms) 1000)))))

(defn start-audio-capture-file!
  "Reads from WAV file in 20ms chunks.
   Returns a channel that will receive byte arrays of audio data."
  [{:audio-in/keys [file-path]
    :or {file-path "input.wav"}}]
  (let [audio-file (io/resource file-path)
        audio-stream (AudioSystem/getAudioInputStream audio-file)
        audio-format (.getFormat audio-stream)
        chunk-size (calculate-chunk-size 20 audio-format)
        buffer (byte-array chunk-size)
        out-ch (a/chan 1024)
        running? (atom true)]

    (future
      (try
        (while (and @running?
                    (pos? (.read audio-stream buffer 0 chunk-size)))
          (let [audio-data (Arrays/copyOf buffer chunk-size)]
            (a/offer! out-ch audio-data)))
        (catch Exception e
          (a/put! out-ch {:error e}))
        (finally
          (.close audio-stream)
          (a/close! out-ch))))

    {:audio-chan out-ch
     :stop-fn #(do (reset! running? false)
                   (a/close! out-ch))}))

(defn line-supported?
  [^DataLine$Info info]
  (AudioSystem/isLineSupported info))

(defn open-microphone!
  "Opens the microphone with specified format. Returns the TargetDataLine."
  [^AudioFormat format]
  (let [info (line-info :target format)
        line (line info)]
    (when-not (line-supported? info)
      (throw (ex-info "Audio line not supported"
                      {:format format})))
    (open! line format)
    (start! line)
    line))

(defn- frame-buffer-size
  "Get read buffer size based on the sample rate for input"
  [sample-rate]
  (* 2 (/ sample-rate 100)))

(defn start-audio-capture!
  "Starts capturing audio from the microphone.
   Returns a channel that will receive byte arrays of audio data.

   Options:
   :sample-rate - The sample rate in Hz (default: 16000)
   :channels - Number of audio channels (default: 1)"
  ([] (start-audio-capture! {}))
  ([{:audio-in/keys [sample-rate sample-size-bits channels]
     :or {sample-rate 16000
          channels 1
          sample-size-bits 16}}]
   (let [buffer-size (frame-buffer-size sample-rate)
         af (audio-format sample-rate sample-size-bits channels)
         line (open-microphone! af)
         out-ch (a/chan 1024)
         buffer (byte-array buffer-size)
         running? (atom true)]

     ;; Start capture loop in a separate thread
     (future
       (try
         (while @running?
           (let [bytes-read (read! line buffer 0 buffer-size)]
             (when (pos? bytes-read)
               ;; Copy only the bytes that were read
               (let [audio-data (Arrays/copyOfRange buffer 0 bytes-read)]
                 ;; Put data on channel, but don't block if channel is full
                 (a/offer! out-ch audio-data)))))
         (catch Exception e
           (a/put! out-ch {:error e}))
         (finally
           (.stop ^TargetDataLine line)
           (.close ^TargetDataLine line)
           (a/close! out-ch))))

     ;; Return a map with the channel and a stop function
     {:audio-chan out-ch
      :stop-fn #(do (a/close! out-ch)
                    (reset! running? false))})))

(defmethod process-frame :transport/local-audio
  [processor-type pipeline _ frame]
  (case (:frame/type frame)
    :system/start
    (do
      (t/log! :debug "Starting audio capture")
      (let [{:keys [audio-chan stop-fn]} (start-audio-capture-file! (:pipeline/config @pipeline))]
        ;; Store stop-fn in state for cleanup
        (swap! pipeline assoc-in [:transport/local-audio :stop-fn] stop-fn)
        ;; Start sending audio frames
        (a/go-loop []
          (when-let [data (a/<! audio-chan)]
            (a/>! (:pipeline/main-ch @pipeline) (frames/audio-input-frame data))
            (recur)))))

    :system/stop
    (do
      (t/log! :debug "Stopping audio capture")
      (when-let [stop-fn (get-in @pipeline [:transport/local-audio :stop-fn])]
        (stop-fn)))
    (close-processor! pipeline processor-type))
  nil)

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/transport/serializers.clj`:

```clj
(ns voice-fn.transport.serializers
  (:require
   [voice-fn.frames :as frames]
   [voice-fn.transport.protocols :as p]
   [voice-fn.utils.core :as u]))

;; Example Twilio serializer
(defn make-twilio-serializer [stream-sid]
  (reify
    p/FrameSerializer
    (serialize-frame [_ frame]
      ;; Convert pipeline frame to Twilio-specific format
      (case (:frame/type frame)
        :audio/output (u/json-str {:event "media"
                                   :streamSid stream-sid
                                   :media {:payload (:data frame)}})
        nil))

    (deserialize-frame [_ raw-data]
      ;; Convert Twilio message to pipeline frame
      (let [data (u/parse-if-json raw-data)]
        (case (:event data)
          ;; TODO more cases
          "media" (frames/audio-input-frame (u/decode-base64 (:payload (:media data))))
          nil)))))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/transport/async.clj`:

```clj
(ns voice-fn.transport.async
  (:require
   [clojure.core.async :as a]
   [taoensso.telemere :as t]
   [voice-fn.frames :as frames]
   [voice-fn.pipeline :as pipeline]
   [voice-fn.transport.protocols :as tp]))

(defn- input->frame
  ([input]
   (if (frames/audio-input-frame? input)
     input
     (throw (ex-info "Input is not a valid input frame. Please provide a serializer"
                     {:input input
                      :cause :error/transport-missing-serializer}))))
  ([input serializer]
   (if (nil? serializer)
     (input->frame input)
     (if (frames/audio-input-frame? input)
       input
       (tp/deserialize-frame serializer input)))))

(defmethod pipeline/process-frame :transport/async-input
  [processor-type pipeline _ frame]
  (let [{:transport/keys [in-ch serializer]} (:pipeline/config @pipeline)
        running? (atom false)]
    (case (:frame/type frame)
      :system/start
      (do
        (t/log! {:level :info
                 :id processor-type} "Staring transport input")
        (reset! running? true)
        (a/go-loop []
          (when running?
            (when-let [input (a/<! in-ch)]
              (when-let [input-frame (try
                                       (input->frame input serializer)
                                       (catch clojure.lang.ExceptionInfo e
                                         (let [data (merge (ex-data e)
                                                           {:message (ex-message e)})]
                                           (a/>! (:pipeline/main-ch @pipeline) (frames/error-frame data)))))]
                (a/>! (:pipeline/main-ch @pipeline) input-frame))
              (recur)))))
      :system/stop
      (t/log! {:level :info
               :id processor-type} "Stopping transport input")
      (reset! running? false))))

(defmethod pipeline/process-frame :transport/async-output
  [type pipeline _ frame]
  (t/log! {:level :debug
           :id type}
    ["Output frame" (:frame/data frame)])
  (let [{:transport/keys [out-ch serializer]} (:pipeline/config @pipeline)]
    (case (:frame/type frame)
      :audio/output (when-let [output (if serializer
                                        (tp/serialize-frame serializer frame)
                                        frame)]
                      (a/put! out-ch output))
      :system/stop (a/close! out-ch))))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/frames.clj`:

```clj
(ns voice-fn.frames)

(defrecord BaseFrame [type data ts])

(defn frame? [frame]
  (instance? BaseFrame frame))

(defn create-frame
  [type data]
  (let [ts (System/currentTimeMillis)]
    ;; Using namespaced keywords for convenience sicne records don't support
    ;; namespaced params by default
    (map->BaseFrame {:type type
                     :frame/type type
                     :data data
                     :frame/data data
                     :ts ts
                     :frame/ts ts})))

(def start-frame (create-frame :system/start nil))

(def stop-frame (create-frame :system/stop nil))

(defmacro defframe
  "Define a frame creator function and its predicate.
   Usage: (defframe audio-input :audio/raw-input \"Doc string\")"
  [name type docstring]
  `(do
     (defn ~name
       ~docstring
       [data#]
       (create-frame ~type data#))

     (defn ~(symbol (str name "?"))
       [frame#]
       (and (frame? frame#) (= ~type (:frame/type frame#))))))

(defframe audio-input-frame :audio/raw-input "User audio input frame. Put on the pipeline by the input transport. Data is
  bynary")

(defframe audio-output-frame :audio/output "Audio frame to be played back to the user through output transport. Generated
  by text to speech processors or multi modal (with voice capabilities) LLM processors. Data is binary")

(defframe text-input-frame :text/input "Frame usually outputted by a transcription processor. Serves as input for text
  LLM processors.")

(defframe llm-output-text-sentence-frame :llm/output-text-sentence
  "Frame generated by a sentence assembler that takes in chunks from a LLM processor")

(defframe llm-output-text-chunk-frame :llm/output-text-chunk
  "Frame outputted by text based streaming LLMs")

(defframe error-frame :system/error "General error")

(defframe elevenlabs-audio-chunk-frame :elevenlabs/audio-chunk "Elevenlabs doesn't send full json, sometimes it just sends partial JSON that needs to be assembled")

(defframe llm-user-context-added-frame :llm/user-context-added "Frame sent after the user context was appended to the AI context. Used to sync the llm aggregator with the llm processor pipeline")

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/pipeline.clj`:

```clj
(ns voice-fn.pipeline
  (:require
   [clojure.core.async :as a :refer [<! >! chan go-loop]]
   [malli.core :as m]
   [malli.error :as me]
   [malli.transform :as mt]
   [taoensso.telemere :as t]
   [voice-fn.frames :as frames]
   [voice-fn.schema :as schema]
   [voice-fn.secrets :refer [secret]]))

(defmulti processor-schema
  "Returns the malli schema for a processor type's configuration"
  {:arglists '([processor-type])}
  (fn [processor-type] processor-type))

(defmethod processor-schema :default
  [_]
  any?)

(defmulti make-processor-config
  "Create the configuration for the processor based on the pipeline
  configuration. Used when the final configuration for a processor requires
  information from the global pipeline configuration. ex: audio-in encoding,
  pipeline language, etc.

  - type - the processor type

  - pipeline-config - the global config of the pipeline. It contains config such
  as the input audio-encoding, pipeline language, input and output channels and
  more. See `voice-fn.pipeline/PipelineConfigSchema`.

  - processor-config - the config of the processor as specified in the list of
  processors from the pipeline"
  {:arglists '([type pipeline-config processor-config])}
  (fn [type _pipeline-config _processor-config]
    type))

(defmethod make-processor-config :default
  [type _ processor-config]
  (m/decode (processor-schema type) processor-config mt/default-value-transformer))

(defmulti process-frame
  "Process a frame from the pipeline.
  - processor-type - type of processor: `:transport/local-audio` | `:transcription/deepgram`
  - pipeline - atom containing the state of the pipeline.
  - config - pipeline config
  - frame - the frame to be processed by the processor"
  {:arglists '([processor-type pipeline config frame])}
  (fn [processor-type _state _config _frame]
    processor-type))

(def PipelineConfigSchema
  [:map
   [:audio-in/sample-rate {:default 16000} schema/SampleRate]
   [:audio-in/channels {:default 1} schema/AudioChannels]
   [:audio-in/encoding {:default :pcm-signed} schema/AudioEncoding]
   [:audio-in/sample-size-bits {:default 16} schema/SampleSizeBits]
   [:audio-out/sample-rate {:default 16000} schema/SampleRate]
   [:audio-out/channels {:default 1} schema/AudioChannels]
   [:audio-out/encoding {:default :pcm-signed} schema/AudioEncoding]
   [:audio-out/sample-size-bits {:default 16} schema/SampleSizeBits]
   [:pipeline/language schema/Language]
   [:llm/context schema/LLMContext]
   [:transport/in-ch schema/Channel]
   [:transport/out-ch schema/Channel]])

(defn validate-pipeline
  "Validates the pipeline configuration and all processor configs.
   Returns a map with :valid? boolean and :errors containing any validation errors.

   Example return for valid config:
   {:valid? true}

   Example return for invalid config:
   {:valid? false
    :errors {:pipeline {...}           ;; Pipeline config errors
             :processors [{:type :some/processor
                          :errors {...}}]}} ;; Processor specific errors"
  [{pipeline-config :pipeline/config
    processors :pipeline/processors}]
  (let [;; Validate main pipeline config
        pipeline-valid? (m/validate PipelineConfigSchema pipeline-config)
        pipeline-errors (when-not pipeline-valid?
                          (me/humanize (m/explain PipelineConfigSchema pipeline-config)))

        ;; Validate each processor's config
        processor-results
        (for [{:processor/keys [type config] :as processor} processors]
          (let [schema (processor-schema type)
                processor-config (make-processor-config type pipeline-config config)
                processor-valid? (m/validate schema processor-config)
                processor-errors (when-not processor-valid?
                                   (me/humanize (m/explain schema processor-config)))]
            {:type type
             :valid? processor-valid?
             :errors processor-errors}))

        ;; Check if any processors are invalid
        invalid-processors (filter (comp not :valid?) processor-results)

        ;; Combine all validation results
        all-valid? (and pipeline-valid?
                        (empty? invalid-processors))]

    (cond-> {:valid? all-valid?}

      ;; Add pipeline errors if any
      (not pipeline-valid?)
      (assoc-in [:errors :pipeline] pipeline-errors)

      ;; Add processor errors if any
      (seq invalid-processors)
      (assoc-in [:errors :processors]
                (keep #(when-not (:valid? %)
                         {:type (:type %)
                          :errors (:errors %)})
                      processor-results)))))

(comment
  (def in (a/chan 1))
  (def out (a/chan 1))
  (def test-pipeline-config {:pipeline/config {:audio-in/sample-rate 8000
                                               :audio-in/encoding :ulaw
                                               :audio-in/channels 1
                                               :audio-in/sample-size-bits 8
                                               :audio-out/sample-rate 8000
                                               :audio-out/encoding :ulaw
                                               :audio-out/sample-size-bits 8
                                               :audio-out/channels 1
                                               :pipeline/language :ro
                                               :llm/context [{:role "system" :content  "Ești un agent vocal care funcționează prin telefon. Răspunde doar în limba română și fii succint. Inputul pe care îl primești vine dintr-un sistem de speech to text (transcription) care nu este intotdeauna eficient și poate trimite text neclar. Cere clarificări când nu ești sigur pe ce a spus omul."}]
                                               :transport/in-ch in
                                               :transport/out-ch out}
                             :pipeline/processors
                             [{:processor/type :transport/twilio-input
                               :processor/accepted-frames #{:system/start :system/stop}
                               :processor/generates-frames #{:audio/raw-input}}
                              {:processor/type :transcription/deepgram
                               :processor/accepted-frames #{:system/start :system/stop :audio/raw-input}
                               :processor/generates-frames #{:text/input}
                               :processor/config {:transcription/api-key (secret [:deepgram :api-key])
                                                  :transcription/interim-results? true
                                                  :transcription/punctuate? false
                                                  :transcription/vad-events? true
                                                  :transcription/smart-format? true
                                                  :transcription/model :nova-2}}
                              {:processor/type :llm/context-aggregator
                               :processor/accepted-frames #{:llm/output-text-sentence :text/input}
                               :processor/generates-frames #{:llm/user-context-added}}
                              {:processor/type :llm/openai
                               :processor/accepted-frames #{:llm/user-context-added}
                               :processor/generates-frames #{:llm/output-text-chunk}
                               :processor/config {:llm/model "gpt-4o-mini"
                                                  :openai/api-key (secret [:openai :new-api-sk])}}
                              {:processor/type :log/text-input
                               :processor/accepted-frames #{:text/input}
                               :processor/generates-frames #{}
                               :processor/config {}}
                              {:processor/type :llm/sentence-assembler
                               :processor/accepted-frames #{:system/stop :llm/output-text-chunk}
                               :processor/generates-frames #{:llm/output-text-sentence}
                               :processor/config {:sentence/end-matcher #"[.?!;:]"}}
                              {:processor/type :tts/elevenlabs
                               :processor/accepted-frames #{:system/stop :system/start :llm/output-text-sentence}
                               :processor/generates-frames #{:audio/output :elevenlabs/audio-chunk}
                               :processor/config {:elevenlabs/api-key (secret [:elevenlabs :api-key])
                                                  :elevenlabs/model-id "eleven_flash_v2_5"
                                                  :elevenlabs/voice-id "7sJPxFeMXAVWZloGIqg2"
                                                  :voice/stability 0.5
                                                  :voice/similarity-boost 0.8
                                                  :voice/use-speaker-boost? true}}
                              {:processor/type :elevenlabs/audio-assembler
                               :processor/accepted-frames #{:elevenlabs/audio-chunk}
                               :processor/generates-frames #{:audio/output}}
                              {:processor/type :transport/async-output
                               :processor/accepted-frames #{:audio/output :system/stop}
                               :generates/frames #{}}]})

  (validate-pipeline test-pipeline-config)

  (make-processor-config :transcription/deepgram {:audio-in/sample-rate 8000
                                                  :audio-in/encoding :ulaw
                                                  :audio-in/channels 1
                                                  :audio-in/sample-size-bits 8
                                                  :audio-out/sample-rate 8000
                                                  :audio-out/encoding :ulaw
                                                  :audio-out/sample-size-bits 8
                                                  :audio-out/channels 1
                                                  :pipeline/language :ro}
                         {:transcription/api-key (secret [:deepgram :api-key])
                          :transcription/interim-results? true
                          :transcription/punctuate? false
                          :transcription/vad-events? true
                          :transcription/smart-format? true
                          :transcription/model :nova-2})

  ,)

(defn enrich-processor
  [pipeline-config processor]
  (assoc-in processor [:processor/config] (make-processor-config (:processor/type processor) pipeline-config (:processor/config processor))))

(defn enrich-processors
  "Add pipeline configuration to each processor config based on `make-processor-config`"
  [pipeline]
  (merge pipeline
         {:pipeline/processors (mapv (partial enrich-processor (:pipeline/config pipeline)) (:pipeline/processors pipeline))}))

;; Pipeline creation logic here
(defn create-pipeline
  "Creates a new pipeline from the provided configuration.

   Throws ExceptionInfo with :type :invalid-pipeline-config when the configuration
   is invalid. The exception data will contain :errors with detailed validation
   information.

   Returns an atom containing the initialized pipeline state."
  [pipeline-config]
  (let [validation-result (validate-pipeline pipeline-config)]
    (if (:valid? validation-result)
      (let [main-ch (chan 1024)
            main-pub (a/pub main-ch :frame/type)
            pipeline (atom (merge {:pipeline/main-ch main-ch
                                   :pipeline/main-pub main-pub}
                                  (enrich-processors pipeline-config)))]
        ;; Start each processor
        (doseq [{:processor/keys [type accepted-frames]} (:pipeline/processors pipeline-config)]
          (let [processor-ch (chan 1024)]
            ;; Tap into main channel, filtering for accepted frame types
            (doseq [frame-type accepted-frames]
              (a/sub main-pub frame-type processor-ch))
            (swap! pipeline assoc-in [type :processor/in-ch] processor-ch)))
        pipeline)
      ;; Throw detailed validation error
      (throw (ex-info "Invalid pipeline configuration"
                      {:type :pipeline/invalid-configuration
                       :errors (:errors validation-result)})))))

(defn start-pipeline!
  [pipeline]
  (t/log! :debug "Starting pipeline")
  (a/put! (:pipeline/main-ch @pipeline) {:frame/type :system/start})
  ;; Start each processor
  (doseq [{:processor/keys [type] :as processor} (:pipeline/processors @pipeline)]
    (go-loop []
      (when-let [frame (<! (get-in @pipeline [type :processor/in-ch]))]
        (when-let [result (process-frame type pipeline processor frame)]
          ;; Put results back on main channel if the processor returned frames
          (when (frames/frame? result)
            (>! (:pipeline/main-ch @pipeline) result)))
        (recur)))))

(defn stop-pipeline!
  [pipeline]
  (t/log! :debug "Stopping pipeline")
  (t/log! :debug ["Conversation so far" (get-in @pipeline [:pipeline/config :llm/context])])
  (a/put! (:pipeline/main-ch @pipeline) {:frame/type :system/stop}))

(defn close-processor!
  [pipeline type]
  (t/log! {:level :debug
           :id type} "Closing processor")
  (a/close! (get-in @pipeline [type :processor/in-ch])))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/schema.clj`:

```clj
(ns voice-fn.schema
  (:require
   [clojure.core.async.impl.protocols :as async-protocols]
   [malli.core :as m]
   [malli.error :as me]
   [malli.transform :as mt]))

(defn flex-enum
  "Creates a flexible enum that accepts both keywords and their string versions.
   Input: coll of keywords or string
   Output: Malli enum schema that accepts both forms

   Example:
   (flex-enum [:nova-2 \"nova-2-general\" :gpt-4 \"gpt-4-turbo\"])"
  ([vals]
   (flex-enum {} vals))
  ([meta vals]
   (into [:enum meta] (distinct (mapcat (fn [v]
                                          [(name v)
                                           (keyword v)]) vals)))))

(defn regex?
  [input]
  (instance? java.util.regex.Pattern input))

(defn safe-coerce
  "Coerce to value without throwing error but instead "
  [schema value transformer]
  (m/coerce schema value transformer identity (partial (comp me/humanize :explain))))

(def SampleRate
  [:enum
   {:description "Audio sample rate in Hz"
    :error/message "Invalid sample rate"}
   8000                                 ; Low quality telephony
   11025                                ; Quarter of CD quality
   12000                                ; Some voice recording
   16000                                ; Common for speech recognition
   22050                                ; Half CD quality
   24000                                ; Some digital audio
   32000                                ; Digital broadcasting
   44100                                ; CD quality
   48000                                ; Professional audio/DAT
   88200                                ; High-res audio (2× 44.1)
   96000                                ; High-res audio/video
   176400                               ; Highest-res audio (4× 44.1)
   192000])

(def AudioEncoding
  [:enum
   :alaw
   :ulaw
   :pcm-signed
   :pcm-unsigned
   :pcm-float])

(def AudioChannels
  [:enum
   {:default 1
    :description "Type of audio (mono/stereo)"
    :error/message "Invalid audio channels"}
   1   ;; mono
   2]) ;; stereo

(def SampleSizeBits
  [:enum {:description "Audio sample size in bits"
          :title "Sample Size"
          :json-schema/example 16}
   8 16 24 32])

(def Language
  (flex-enum
    {:description "Language codes including regional variants"}
    [;; Afrikaans
     "af" "af-ZA"

     ;; Amharic
     "am" "am-ET"

     ;; Arabic
     "ar" "ar-AE" "ar-BH" "ar-DZ" "ar-EG" "ar-IQ" "ar-JO" "ar-KW"
     "ar-LB" "ar-LY" "ar-MA" "ar-OM" "ar-QA" "ar-SA" "ar-SY" "ar-TN" "ar-YE"

     ;; Assamese
     "as" "as-IN"

     ;; Azerbaijani
     "az" "az-AZ"

     ;; Bulgarian
     "bg" "bg-BG"

     ;; Bengali
     "bn" "bn-BD" "bn-IN"

     ;; Bosnian
     "bs" "bs-BA"

     ;; Catalan
     "ca" "ca-ES"

     ;; Czech
     "cs" "cs-CZ"

     ;; Welsh
     "cy" "cy-GB"

     ;; Danish
     "da" "da-DK"

     ;; German
     "de" "de-AT" "de-CH" "de-DE"

     ;; Greek
     "el" "el-GR"

     ;; English
     "en" "en-AU" "en-CA" "en-GB" "en-HK" "en-IE" "en-IN" "en-KE"
     "en-NG" "en-NZ" "en-PH" "en-SG" "en-TZ" "en-US" "en-ZA"

     ;; Spanish
     "es" "es-AR" "es-BO" "es-CL" "es-CO" "es-CR" "es-CU" "es-DO"
     "es-EC" "es-ES" "es-GQ" "es-GT" "es-HN" "es-MX" "es-NI" "es-PA"
     "es-PE" "es-PR" "es-PY" "es-SV" "es-US" "es-UY" "es-VE"

     ;; Estonian
     "et" "et-EE"

     ;; Basque
     "eu" "eu-ES"

     ;; Persian
     "fa" "fa-IR"

     ;; Finnish
     "fi" "fi-FI"

     ;; Filipino
     "fil" "fil-PH"

     ;; French
     "fr" "fr-BE" "fr-CA" "fr-CH" "fr-FR"

     ;; Irish
     "ga" "ga-IE"

     ;; Galician
     "gl" "gl-ES"

     ;; Gujarati
     "gu" "gu-IN"

     ;; Hebrew
     "he" "he-IL"

     ;; Hindi
     "hi" "hi-IN"

     ;; Croatian
     "hr" "hr-HR"

     ;; Hungarian
     "hu" "hu-HU"

     ;; Armenian
     "hy" "hy-AM"

     ;; Indonesian
     "id" "id-ID"

     ;; Icelandic
     "is" "is-IS"

     ;; Italian
     "it" "it-IT"

     ;; Inuktitut
     "iu-Cans" "iu-Cans-CA" "iu-Latn" "iu-Latn-CA"

     ;; Japanese
     "ja" "ja-JP"

     ; Javanese
     "jv" "jv-ID"

     ;; Georgian
     "ka" "ka-GE"

     ;; Kazakh
     "kk" "kk-KZ"

     ;; Khmer
     "km" "km-KH"

     ;; Kannada
     "kn" "kn-IN"

     ;; Korean
     "ko" "ko-KR"

     ;; Lao
     "lo" "lo-LA"

     ;; Lithuanian
     "lt" "lt-LT"

     ;; Latvian
     "lv" "lv-LV"

     ;; Macedonian
     "mk" "mk-MK"

     ;; Malayalam
     "ml" "ml-IN"

     ;; Mongolian
     "mn" "mn-MN"

     ;; Marathi
     "mr" "mr-IN"

     ;; Malay
     "ms" "ms-MY"

     ;; Maltese
     "mt" "mt-MT"

     ;; Burmese
     "my" "my-MM"

     ;; Norwegian
     "nb" "nb-NO" "no"

     ;; Nepali
     "ne" "ne-NP"

     ;; Dutch
     "nl" "nl-BE" "nl-NL"

     ;; Odia
     "or" "or-IN"

     ;; Punjabi
     "pa" "pa-IN"

     ;; Polish
     "pl" "pl-PL"

     ;; Pashto
     "ps" "ps-AF"

     ;; Portuguese
     "pt" "pt-BR" "pt-PT"

     ;; Romanian
     "ro" "ro-RO"

     ;; Russian
     "ru" "ru-RU"

     ;; Sinhala
     "si" "si-LK"

     ;; Slovak
     "sk" "sk-SK"

     ;; Slovenian
     "sl" "sl-SI"

     ;; Somali
     "so" "so-SO"

     ;; Albanian
     "sq" "sq-AL"

     ;; Serbian
     "sr" "sr-RS" "sr-Latn" "sr-Latn-RS"

     ;; Sundanese
     "su" "su-ID"

     ;; Swedish
     "sv" "sv-SE"

     ;; Swahili
     "sw" "sw-KE" "sw-TZ"

     ;; Tagalog
     "tl"

     ;; Tamil
     "ta" "ta-IN" "ta-LK" "ta-MY" "ta-SG"

     ;; Telugu
     "te" "te-IN"

     ;; Thai
     "th" "th-TH"

     ;; Turkish
     "tr" "tr-TR"

     ;; Ukrainian
     "uk" "uk-UA"

     ;; Urdu
     "ur" "ur-IN" "ur-PK"

     ;; Uzbek
     "uz" "uz-UZ"

     ;; Vietnamese
     "vi" "vi-VN"

     ;; Wu Chinese
     "wuu" "wuu-CN"

     ;; Yue Chinese
     "yue" "yue-CN"

     ;; Chinese
     "zh" "zh-CN" "zh-CN-guangxi" "zh-CN-henan" "zh-CN-liaoning"
     "zh-CN-shaanxi" "zh-CN-shandong" "zh-CN-sichuan" "zh-HK" "zh-TW"

     ;; Xhosa
     "xh"

     ;; Zulu
     "zu" "zu-ZA"]))

(def LLMContextessageRole
  (flex-enum
    {:description "Role of the message participant"
     :error/message "Role must be system, assistant, or user"}
    ["system"
     "assistant"
     "user"]))

(def LLMContextMessage
  [:map
   {:description "Message from the LLM Context"
    :error/message "Invalid message format"
    :closed true} ;; No additional fields allowed
   [:role LLMContextessageRole]
   [:content [:string {:min 1}]]])

(def LLMContext
  [:vector
   {:min 1
    :max 100 ;; Optional max limit
    :description "Vector of context messages"
    :error/message "Must have at least one context message"}
   LLMContextMessage])

(def Channel
  [:fn
   {:error/message "Must be a core.async channel"
    :description "core.async channel"}
   #(satisfies? async-protocols/Channel %)])

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/secrets.clj`:

```clj
(ns voice-fn.secrets
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(defn- secret-map
  []
  (edn/read-string (slurp (io/resource "secrets.edn"))))

(defn secret
  [path]
  (get-in (secret-map) path))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/core.clj`:

```clj
(ns voice-fn.core
  (:require
   [clojure.core.async :as a]
   [ring.websocket.protocols :as wsp]
   [taoensso.telemere :as t]
   [voice-fn.pipeline :as pipeline]
   [voice-fn.processors.deepgram]
   [voice-fn.processors.openai]
   [voice-fn.secrets :refer [secret]]
   [voice-fn.transport.async]
   [voice-fn.transport.local.audio]))

(def local-transcription-log-pipeline
  {:pipeline/config {:audio-in/sample-rate 16000
                     :audio-in/encoding :pcm-signed
                     :audio-in/channels 1
                     :audio-in/file-path "test-voice.wav"
                     :audio-in/sample-size-bits 16 ;; 2 bytes
                     :audio-out/sample-rate 24000
                     :audio-out/bitrate 96000
                     :audio-out/sample-size-bits 16
                     :audio-out/channels 1
                     :pipeline/language :ro}
   :pipeline/processors [{:processor/type :transport/local-audio
                          :processor/accepted-frames #{:system/start :system/stop}
                          :processor/generates-frames #{:audio/raw-input}}
                         {:processor/type :transcription/deepgram
                          :processor/accepted-frames #{:system/start :system/stop :audio/raw-input}
                          :processor/generates-frames #{:text/input}
                          :processor/config {:transcription/api-key (secret [:deepgram :api-key])
                                             :transcription/interim-results? false
                                             :transcription/punctuate? false
                                             :transcription/model :nova-2}}
                         {:processor/type :log/text-input
                          :processor/accepted-frames #{:text/input}
                          :processor/config {}}]})

(def ws-client (a/chan 10))
(def ws-server (a/chan 10))

(def mock-socket (reify
                   wsp/Socket
                   (-send [_ message]
                     (a/put! ws-client [:send message]))
                   (-close [_ code reason]
                     (a/>!! ws-client [:close code reason]))
                   wsp/AsyncSocket
                   (-send-async [_ mesg succeed _]
                     (a/>!! ws-client [:send mesg])
                     (succeed))))

(def async-echo-pipeline
  {:pipeline/config {:audio-in/sample-rate 8000
                     :audio-in/encoding :ulaw
                     :audio-in/channels 1
                     :audio-in/sample-size-bits 8
                     :audio-out/sample-rate 8000
                     :audio-out/encoding :ulaw
                     :audio-out/bitrate 64000
                     :audio-out/sample-size-bits 8
                     :audio-out/channels 1
                     :pipeline/language :ro}
   :pipeline/processors [;; Transport in
                         {:processor/type :transport/async-input
                          :processor/accepted-frames #{:system/start :system/stop}
                          :processor/generates-frames #{:audio/raw-input}}
                         ;; Transcription
                         {:processor/type :transcription/deepgram
                          :processor/accepted-frames #{:system/start :system/stop :audio/raw-input}
                          :processor/generates-frames #{:text/input}
                          :processor/config {:transcription/api-key (secret [:deepgram :api-key])
                                             :transcription/interim-results? false
                                             :transcription/punctuate? false
                                             :transcription/model :nova-2}}
                         ;; LLM
                         {:processor/type :llm/openai
                          :processor/accepted-frames #{:system/stop :text/input}
                          :processor/generates-frames #{:llm/output-text-chunk}
                          :processor/config {:llm/model "gpt-4o-mini"
                                             :llm/messages [{:role "system" :content "You are a helpful assistant"}]
                                             :openai/api-key (secret [:openai :new-api-key])}}

                         ;; Sentence assembler
                         {:processor/type :llm/sentence-assembler
                          :processor/accepted-frames #{:system/stop :system/start :llm/output-text-chunk}
                          :processor/generates-frames #{:llm/output-text-sentence}
                          :processor/config {:sentence/end-matcher #"[.?!]"}}
                         ;; TTS
                         {:processor/type :tts/elevenlabs
                          :processor/accepted-frames #{:system/stop :system/start :llm/output-text-sentence}
                          :processor/generates-frames #{:audio/output}
                          :processor/config {:elevenlabs/api-key (secret [:elevenlabs :api-key])
                                             :elevenlabs/model-id "eleven_flash_v2_5"
                                             :elevenlabs/voice-id "7sJPxFeMXAVWZloGIqg2"
                                             :voice/stability 0.5
                                             :voice/similarity-boost 0.8
                                             :voice/use-speaker-boost? true}}
                         ;; Logger
                         {:processorvoi/type :log/text-input
                          :processor/accepted-frames #{:text/input}
                          :processor/generates-frames #{}
                          :processor/config {}}
                         ;; Transport out
                         {:processor/type :transport/async-output
                          :processor/accepted-frames #{:audio/output :system/stop}
                          :generates/frames #{}}]})

(defmethod pipeline/process-frame :log/text-input
  [_ _ _ frame]
  (t/log! {:level :info
           :id :log/text-input} ["Frame" (:frame/data frame)]))

(t/set-min-level! :debug)

(comment
  (def p (pipeline/create-pipeline async-echo-pipeline))

  @p

  (pipeline/start-pipeline! p)
  (pipeline/stop-pipeline! p)

  ,)

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/utils/core.clj`:

```clj
(ns voice-fn.utils.core
  (:require
   [clojure.string :as str]
   [jsonista.core :as json])
  (:import
   (java.util Base64)))

(defmulti encode-base64 (fn [s] (class s)))

(defmethod encode-base64 String
  [s]
  (let [encoder (Base64/getEncoder)]
    (.encodeToString encoder (.getBytes s "UTF-8"))))

(defmethod encode-base64 (Class/forName "[B")
  [bytes]
  (let [encoder (Base64/getEncoder)]
    (.encodeToString encoder bytes)))

(defn decode-base64
  [s]
  (let [decoder (Base64/getDecoder)]
    (.decode decoder s)))

(defonce json-object-mapper (json/object-mapper {:decode-key-fn true}))

(defn parse-if-json
  "Parses a string as JSON if possible, otherwise returns the string."
  [s]
  (try
    (json/read-value s json-object-mapper)
    (catch Exception _
      s)))

(defn json-str
  [m]
  (json/write-value-as-string m))

(defn search-params
  [url]
  (let [params (second (str/split url #"\?"))
        param-list (if params (str/split params #"\&") [])]
    (reduce #(let [[k v] (str/split %2 #"\=")]
               (assoc %1 (keyword k) v)) {} param-list)))

(defn strip-search-params
  [url]
  (first (str/split url #"\?")))

(defn append-search-params
  [url search-params-m]
  (let [search-params (merge (search-params url)
                             search-params-m)
        search (->> (map  (fn [[k v]] (str (name k) "=" (if (keyword? v) (name v) v))) search-params)
                    (str/join #"&")
                    (str "?"))]
    (str (strip-search-params url) search)))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/processors/llm_sentence_assembler.clj`:

```clj
(ns voice-fn.processors.llm-sentence-assembler
  (:require
   [clojure.core.async :as a]
   [taoensso.telemere :as t]
   [voice-fn.frames :as frames]
   [voice-fn.pipeline :as pipeline]
   [voice-fn.schema :as schema]))

(def default-end-sentence-matcher #"[.?!;:]")

(def SentenceAssemblerConfig
  [:map
   {:closed true
    :description "Configuration for sentence assembly and detection"}
   [:sentence/end-matcher
    [:fn
     {:error/message "Must be a valid regex patter (java.util.regex.Pattern)"
      :default #"[.?!;:]"
      :description "Regular expression pattern for detecting sentence endings"}
     schema/regex?]]])

(defmethod pipeline/processor-schema :llm/sentence-assembler
  [_]
  SentenceAssemblerConfig)

(defmethod pipeline/process-frame :llm/sentence-assembler
  [processor-type pipeline {:processor/keys [config]} {:frame/keys [data type]}]
  (let [end-sentence-matcher (:sentence/end-matcher config default-end-sentence-matcher)
        sentence (get-in @pipeline [processor-type :sentence] "")]
    (case type
      :llm/output-text-chunk
      (if (re-find end-sentence-matcher data)
        (let [full-sentence (str sentence data)]
          (t/log! :debug ["Full sentence" full-sentence])
          (swap! pipeline assoc-in [processor-type :sentence] "")
          (a/put! (:pipeline/main-ch @pipeline)
                  (frames/llm-output-text-sentence-frame full-sentence)))
        (swap! pipeline assoc-in [processor-type :sentence] (str sentence data)))
      nil)))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/processors/openai.clj`:

```clj
(ns voice-fn.processors.openai
  (:require
   [clojure.core.async :as a]
   [voice-fn.frames :as frames]
   [voice-fn.pipeline :as pipeline]
   [voice-fn.schema :as schema]
   [wkok.openai-clojure.api :as api]))

(def token-content (comp :content :delta first :choices))

(def OpenAILLMConfigSchema
  [:map
   {:closed true
    :description "OpenAI LLM configuration"}

   [:llm/model
    (schema/flex-enum
      {:description "OpenAI model identifier"
       :error/message "Must be a valid OpenAI model"
       :default "gpt-4o-mini"}
      [;; GPT-4 Models
       "gpt-4"
       "gpt-4-32k"
       "gpt-4-1106-preview"   ;; GPT-4 Turbo
       "gpt-4-vision-preview" ;; GPT-4 Vision
       ;; GPT-3.5 Models
       "gpt-3.5-turbo"
       "gpt-3.5-turbo-16k"
       "gpt-3.5-turbo-1106"
       ;; Base Models
       "babbage-002"
       "davinci-002"
       ;; Include your custom model
       "gpt-4o-mini"])]

   [:openai/api-key
    [:string
     {:description "OpenAI API key"
      :secret true ;; Marks this as sensitive data
      :min 40      ;; OpenAI API keys are typically longer
      :error/message "Invalid OpenAI API key format"}]]])

;; Example validation:
(comment
  (require '[malli.core :as m]
           '[malli.error :as me])
  ;; Valid config
  (m/validate OpenAILLMConfigSchema
              {:llm/model :gpt-4o-mini
               :openai/api-key "sk-12312312312312312312312312312312312312312313..."})
  ;; => true

  ;; Invalid model
  (-> OpenAILLMConfigSchema
      (m/explain {:llm/model "invalid-model"
                  :openai/api-key "sk-..."})
      me/humanize))

(defmethod pipeline/processor-schema :llm/openai
  [_]
  OpenAILLMConfigSchema)

(defmethod pipeline/process-frame :llm/openai
  [_type pipeline processor frame]
  (let [{:llm/keys [model] :openai/keys [api-key]} (:processor/config processor)
        {:llm/keys [context]} (:pipeline/config @pipeline)]
    (case (:frame/type frame)
      :llm/user-context-added (a/pipeline
                                1
                                (:pipeline/main-ch @pipeline)
                                (comp (map token-content) (filter some?) (map frames/llm-output-text-chunk-frame))
                                (api/create-chat-completion {:model model
                                                             :messages context
                                                             :stream true}
                                                            {:api-key api-key
                                                             :version :http-2 :as :stream})))))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/processors/groq.clj`:

```clj
(ns voice-fn.processors.groq
  (:require
   [voice-fn.secrets :refer [secret]]
   [wkok.openai-clojure.api :as api]))

(def groq-api-url "https://api.groq.com/openai/v1")

(comment
  (api/create-completion
    {:model "llama3-8b-8192"
     :messages [{:role "system" :content "You are a helpful assistant."}
                {:role "user" :content "Who won the world series in 2020?"}
                {:role "assistant" :content "The Los Angeles Dodgers won the World Series in 2020."}
                {:role "user" :content "Where was it played?"}]}

    {:api-key (secret [:groq :api-key])

     :api-endpoint groq-api-url}))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/processors/deepgram.clj`:

```clj
(ns voice-fn.processors.deepgram
  (:require
   [clojure.core.async :as a]
   [hato.websocket :as ws]
   [malli.core :as m]
   [malli.error :as me]
   [malli.transform :as mt]
   [taoensso.telemere :as t]
   [voice-fn.frames :as frames]
   [voice-fn.pipeline :refer [close-processor! make-processor-config process-frame processor-schema]]
   [voice-fn.schema :as schema :refer [flex-enum]]
   [voice-fn.utils.core :as u])
  (:import
   (java.nio HeapCharBuffer)))

(def ^:private deepgram-url "wss://api.deepgram.com/v1/listen")

(def deepgram-encoding
  "Mapping from clojure sound encoding to deepgram notation"
  {:pcm-signed :linear16
   :pcm-unsigned :linear16
   :pcm-float :linear16
   :ulaw :mulaw
   :alaw :alaw})

(defn make-deepgram-url
  [{:transcription/keys [interim-results? punctuate? model sample-rate language  vad-events? smart-format? encoding channels]
    :or {interim-results? false
         punctuate? false}}]
  (u/append-search-params deepgram-url {:encoding encoding
                                        :language language
                                        :sample_rate sample-rate
                                        :model model
                                        :smart_format smart-format?
                                        :channels channels
                                        :vad_events vad-events?
                                        :interim_results interim-results?
                                        :punctuate punctuate?}))

(defn transcript?
  [m]
  (= (:event m) "transcript"))

(defn- transcript
  [m]
  (-> m :channel :alternatives first :transcript))

(defn final-transcription?
  [m]
  (and (transcript? m)
       (= (:type m) "final")))

(defn close-connection-payload
  []
  (u/json-str {:type "CloseStream"}))

(declare create-connection-config)

(def max-reconnect-attempts 5)

(defn connect-websocket!
  [type pipeline processor-config]
  (let [current-count (get-in @pipeline [type :websocket/reconnect-count] 0)]
    (if (>= current-count max-reconnect-attempts)
      (t/log! :warn "Maximum reconnection attempts reached for Deepgram")
      (do
        (t/log! :info (str "Attempting to connect to Deepgram (attempt " (inc current-count) "/" max-reconnect-attempts ")"))
        (swap! pipeline update-in [type :websocket/reconnect-count] (fnil inc 0))
        (let [websocket-url (make-deepgram-url processor-config)
              _ (t/log! :debug ["WEBSOCKET URL" websocket-url])
              conn-config (create-connection-config
                            type
                            pipeline
                            processor-config)
              new-conn @(ws/websocket
                          websocket-url
                          conn-config)]
          (swap! pipeline assoc-in [type :websocket/conn] new-conn))))))

(def deepgram-events
  (atom []))

(defn create-connection-config
  [type pipeline processor-config]
  {:headers {"Authorization" (str "Token " (:transcription/api-key processor-config))}
   :on-open (fn [_]
              (t/log! :info "Deepgram websocket connection open"))
   :on-message (fn [_ws ^HeapCharBuffer data _last?]
                 (let [m (u/parse-if-json (str data))
                       trsc (transcript m)]
                   (swap! deepgram-events conj m)
                   (when (and trsc (not= trsc ""))
                     (a/put! (:pipeline/main-ch @pipeline)
                             (frames/text-input-frame trsc)))))
   :on-error (fn [_ e]
               (t/log! :error ["Error" e]))
   :on-close (fn [_ws code reason]
               (t/log! :info ["Deepgram websocket connection closed" "Code:" code "Reason:" reason])
               (when (= code 1011) ;; timeout
                 (connect-websocket! type pipeline processor-config)))})

(defn- close-websocket-connection!
  [type pipeline]
  (when-let [conn (get-in @pipeline [type :websocket/conn])]
    (ws/send! conn (close-connection-payload))
    (ws/close! conn))
  (swap! pipeline update-in [:transcription/deepgram] dissoc :websocket/conn))

(def code-reason
  {1011 :timeout})

(def DeepgramConfig
  [:and
   [:map
    [:transcription/api-key :string]
    [:transcription/model {:default :nova-2-general}
     (flex-enum (into [:nova-2] (map #(str "nova-2-" %) #{"general" "meeting" "phonecall" "voicemail" "finance" "conversationalai" "video" "medical" "drivethru" "automotive" "atc"})))]
    [:transcription/interim-results? {:default true} :boolean]
    [:transcription/channels {:default 1} [:enum 1 2]]
    [:transcription/smart-format? {:default true} :boolean]
    [:transcription/profanity-filter? {:default true} :boolean]
    [:transcription/vad-events? {:default false} :boolean]
    [:transcription/sample-rate schema/SampleRate]
    [:transcription/encoding {:default :linear16} (flex-enum [:linear16 :mulaw :alaw :mp3 :opus :flac :aac])]
    [:transcription/language {:default :en} schema/Language]
    [:transcription/punctuate? {:default false} :boolean]]
   ;; if smart-format is true, no need for punctuate
   [:fn {:error/message "When :transcription/smart-format? is true, :transcription/punctuate? must be false. More details here: https://developers.deepgram.com/docs/smart-format#enable-feature"}
    (fn [{:transcription/keys [smart-format? punctuate?]}]
      (not (and smart-format? punctuate?)))]])

(defn pipeline->deepgram-config
  [value]
  (cond-> {}
    ;; Map sample rate directly
    (:audio-in/sample-rate value)
    (assoc :transcription/sample-rate (:audio-in/sample-rate value))

    ;; Map encoding with conversion
    (:audio-in/encoding value)
    (assoc :transcription/encoding
           (get deepgram-encoding (:audio-in/encoding value)))

    ;; Map language directly
    (:pipeline/language value)
    (assoc :transcription/language (:pipeline/language value))))

(defmethod processor-schema :transcription/deepgram
  [_]
  DeepgramConfig)

(defmethod make-processor-config :transcription/deepgram
  [_ pipeline-config processor-config]
  (m/decode DeepgramConfig
            (merge processor-config
                   (pipeline->deepgram-config pipeline-config))
            (mt/default-value-transformer {::mt/add-optional-keys true})))

(defmethod process-frame :transcription/deepgram
  [type pipeline processor frame]
  (let [on-close! (fn []
                    (t/log! :debug "Stopping transcription engine")
                    (close-websocket-connection! type pipeline)
                    (close-processor! pipeline type))]
    (case (:frame/type frame)
      :system/start
      (do (t/log! :debug "Starting transcription engine")
          (connect-websocket! type pipeline (:processor/config processor)))
      :system/stop (on-close!)

      :audio/raw-input
      (when-let [conn (get-in @pipeline [type :websocket/conn])]
        (ws/send! conn (:data frame))))))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/processors/llm_context_aggregator.clj`:

```clj
(ns voice-fn.processors.llm-context-aggregator
  (:require
   [clojure.core.async :as a]
   [taoensso.telemere :as t]
   [voice-fn.frames :as f]
   [voice-fn.pipeline :as pipeline]))

concat

(defn concat-context
  "Concat to context a new message. If the last message from the context is from
  the same role, concattenate in the same context object.
  (concat-context [{:role :system :content \"Hello\"}] :system \", world\")
  ;; => [{:role :system :content \"Hello, world\"}]
  "
  ([context entry]
   (concat-context context (:role entry) (:content entry)))
  ([context role content]
   (let [last-entry (last context)]
     (if (= (name (:role last-entry)) (name role))
       (into (vec (butlast context))
             [{:role role
               :content (str (:content last-entry) " " content)}])
       (into context
             [{:role role :content content}])))))

(defmethod pipeline/process-frame :llm/context-aggregator
  [_ pipeline _ frame]
  (case (:frame/type frame)
    :llm/output-text-sentence
    (do
      (t/log! :info ["AI sentence" (:frame/data frame)])
      (swap! pipeline update-in [:pipeline/config :llm/context] concat-context "assistant" (:frame/data frame)))
    :text/input
    (do (swap! pipeline update-in [:pipeline/config :llm/context] concat-context "user" (:frame/data frame))
        (a/put! (:pipeline/main-ch @pipeline) (f/llm-user-context-added-frame true)))))

```

`/Users/ovistoica/workspace/voice-fn/core/src/voice_fn/processors/elevenlabs.clj`:

```clj
(ns voice-fn.processors.elevenlabs
  (:require
   [clojure.core.async :as a]
   [hato.websocket :as ws]
   [taoensso.telemere :as t]
   [voice-fn.frames :as f]
   [voice-fn.pipeline :as pipeline]
   [voice-fn.schema :as schema]
   [voice-fn.secrets :as secrets]
   [voice-fn.utils.core :as u])
  (:import
   (java.nio HeapCharBuffer)))

(def ^:private xi-tts-websocket-url "wss://api.elevenlabs.io/v1/text-to-speech/%s/stream-input")

(def elevenlabs-encoding
  "Mapping from clojure sound encoding to elevenlabs format"
  {:ulaw :ulaw_8000
   :mp3 :mp3_44100})

(defn encoding->elevenlabs
  ([format]
   (elevenlabs-encoding format))
  ([format sample-rate]
   (keyword (str (name format) "_" sample-rate))))

(defn make-elevenlabs-url
  [pipeline-config processor-config]
  (let [{:audio-out/keys [encoding sample-rate]
         :pipeline/keys [language]} pipeline-config
        {:elevenlabs/keys [model-id voice-id]
         :or {model-id "eleven_flash_v2_5"
              voice-id "cjVigY5qzO86Huf0OWal"}}
        processor-config]
    (u/append-search-params (format xi-tts-websocket-url voice-id)
                            {:model_id model-id
                             :language_code language
                             :output_format (encoding->elevenlabs encoding sample-rate)})))

(comment
  (make-elevenlabs-url {:audio-in/sample-rate 8000
                        :audio-in/encoding :ulaw
                        :audio-in/channels 1
                        :audio-in/sample-size-bits 8
                        :audio-out/sample-rate 8000
                        :audio-out/encoding :ulaw
                        :audio-out/bitrate 64000
                        :audio-out/sample-size-bits 8
                        :audio-out/channels 1
                        :pipeline/language :ro}
                       {:elevenlabs/api-key (secrets/secret [:elevenlabs :api-key])
                        :elevenlabs/model-id "eleven_flash_v2_5"
                        :elevenlabs/voice-id "7sJPxFeMXAVWZloGIqg2"
                        :voice/stability 0.5
                        :voice/similarity-boost 0.8
                        :voice/use-speaker-boost? true}))

(defn begin-stream-message
  [{:voice/keys [stability similarity-boost use-speaker-boost?]
    :elevenlabs/keys [api-key]
    :or {stability 0.5
         similarity-boost 0.8
         use-speaker-boost? true}}]

  (u/json-str {:text " "
               :voice_settings {:stability stability
                                :similarity_boost similarity-boost
                                :use_speaker_boost use-speaker-boost?}
               :xi_api_key api-key}))

(def close-stream-message
  {:text ""})

(defn text-message
  [text]
  (u/json-str {:text (str text " ")
               :flush true}))

(defn create-connection-config
  [type pipeline processor-config]
  {:on-open (fn [ws]
              (let [configuration (begin-stream-message processor-config)]
                (t/log! :info ["Elevenlabs websocket connection open. Sending configuration message" configuration])
                (ws/send! ws configuration)))
   :on-message (fn [_ws ^HeapCharBuffer data _last?]
                 (a/put! (:pipeline/main-ch @pipeline)
                         (f/elevenlabs-audio-chunk-frame (str data))))
   :on-error (fn [_ e]
               (t/log! :error ["Elevenlabs websocket error" (ex-message e)]))
   :on-close (fn [_ws code reason]
               (t/log! :info ["Elevenlabs websocket connection closed" "Code:" code "Reason:" reason]))})

(def max-reconnect-attempts 5)

(defn connect-websocket!
  [type pipeline processor-config]
  (let [current-count (get-in @pipeline [type :websocket/reconnect-count] 0)]
    (if (>= current-count max-reconnect-attempts)
      (t/log! :warn "Maximum reconnection attempts reached for Elevenlabs")
      (do
        (t/log! :info (str "Attempting to connect to Elevenlabs (attempt " (inc current-count) "/" max-reconnect-attempts ")"))
        (swap! pipeline update-in [type :websocket/reconnect-count] (fnil inc 0))
        (let [conn-config (create-connection-config
                            type
                            pipeline
                            processor-config)
              new-conn @(ws/websocket (make-elevenlabs-url (:pipeline/config @pipeline) processor-config)
                                      conn-config)]
          (swap! pipeline assoc-in [type :websocket/conn] new-conn)
          (t/log! :debug "Elevenlabs connection ready"))))))

(defn- close-websocket-connection!
  [type pipeline]
  (t/log! :info "Closing elevenlabs websocket connection")
  (when-let  [conn (get-in @pipeline [type :websocket/conn])]
    (ws/send! conn (u/json-str close-stream-message))
    (ws/close! conn))

  (swap! pipeline update-in [:tts/elevenlabs] dissoc :websocket/conn))

(def ElevenLabsTTSConfig
  "Configuration for Elevenlabs TextToSpeech service"
  [:map
   {:closed true ;; No additional fields allowed
    :description "ElevenLabs TTS configuration"}
   [:elevenlabs/api-key
    [:string
     {:min 32      ;; ElevenLabs API keys are typically long
      :secret true ;; Marks this as sensitive data
      :description "ElevenLabs API key"}]]
   [:elevenlabs/model-id
    (schema/flex-enum
      {:default "eleven_flash_v2_5"
       :description "ElevenLabs model identifier"}
      ["eleven_multilingual_v2" "eleven_turbo_v2_5" "eleven_turbo_v2" "eleven_monolingual_v1" "eleven_multilingual_v1" "eleven_multilingual_sts_v2" "eleven_flash_v2" "eleven_flash_v2_5" "eleven_english_sts_v2"])]

   [:elevenlabs/voice-id
    [:string
     {:min 20 ;; ElevenLabs voice IDs are fixed length
      :max 20
      :description "ElevenLabs voice identifier"}]]
   [:voice/stability
    [:double
     {:min 0.0
      :max 1.0
      :default 0.5
      :description "Voice stability factor (0.0 to 1.0)"}]]
   [:voice/similarity-boost
    [:double
     {:min 0.0
      :max 1.0
      :default 0.8
      :description "Voice similarity boost factor (0.0 to 1.0)"}]]
   [:voice/use-speaker-boost?
    [:boolean
     {:default true
      :description "Whether to enable speaker boost enhancement"}]]])

(defmethod pipeline/processor-schema :tts/elevenlabs
  [_]
  ElevenLabsTTSConfig)

(defmethod pipeline/process-frame :tts/elevenlabs
  [type pipeline processor frame]
  (case (:frame/type frame)
    :system/start
    (do (t/log! {:level :debug
                 :id type} "Starting text to speech engine")
        (connect-websocket! type pipeline (:processor/config processor)))
    :system/stop (close-websocket-connection! type pipeline)

    :llm/output-text-sentence
    (let [conn (get-in @pipeline [type :websocket/conn])
          xi-message (text-message (:frame/data frame))]
      (t/log! {:level :debug
               :id type} ["Sending websocket payload" xi-message])
      (ws/send! conn xi-message))))

(defmethod pipeline/process-frame :elevenlabs/audio-assembler
  [type pipeline _ frame]
  (let [acc (get-in @pipeline [type :audio-accumulator] "")]
    (case (:frame/type frame)
      :elevenlabs/audio-chunk
      (let [attempt (u/parse-if-json (str acc (:frame/data frame)))]
        (if (map? attempt)
          (when-let [audio (:audio attempt)]
            (swap! pipeline assoc-in [type :audio-accumulator] "")
            (a/put! (:pipeline/main-ch @pipeline)
                    (f/audio-output-frame audio)))
          (do
            (t/log! {:level :debug
                     :id type} ["Accumulating audio chunk" attempt])
            (swap! pipeline assoc-in [type :audio-accumulator] attempt))))
      :system/stop
      (t/log! {:level :debug
               :id type} ["Accumulator at the end" acc]))))

```

`/Users/ovistoica/workspace/voice-fn/LICENSE`:

```
THE ACCOMPANYING PROGRAM IS PROVIDED UNDER THE TERMS OF THIS ECLIPSE PUBLIC
LICENSE ("AGREEMENT"). ANY USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM
CONSTITUTES RECIPIENT'S ACCEPTANCE OF THIS AGREEMENT.

1. DEFINITIONS

"Contribution" means:

a) in the case of the initial Contributor, the initial code and
documentation distributed under this Agreement, and

b) in the case of each subsequent Contributor:

i) changes to the Program, and

ii) additions to the Program;

where such changes and/or additions to the Program originate from and are
distributed by that particular Contributor. A Contribution 'originates' from
a Contributor if it was added to the Program by such Contributor itself or
anyone acting on such Contributor's behalf. Contributions do not include
additions to the Program which: (i) are separate modules of software
distributed in conjunction with the Program under their own license
agreement, and (ii) are not derivative works of the Program.

"Contributor" means any person or entity that distributes the Program.

"Licensed Patents" mean patent claims licensable by a Contributor which are
necessarily infringed by the use or sale of its Contribution alone or when
combined with the Program.

"Program" means the Contributions distributed in accordance with this
Agreement.

"Recipient" means anyone who receives the Program under this Agreement,
including all Contributors.

2. GRANT OF RIGHTS

a) Subject to the terms of this Agreement, each Contributor hereby grants
Recipient a non-exclusive, worldwide, royalty-free copyright license to
reproduce, prepare derivative works of, publicly display, publicly perform,
distribute and sublicense the Contribution of such Contributor, if any, and
such derivative works, in source code and object code form.

b) Subject to the terms of this Agreement, each Contributor hereby grants
Recipient a non-exclusive, worldwide, royalty-free patent license under
Licensed Patents to make, use, sell, offer to sell, import and otherwise
transfer the Contribution of such Contributor, if any, in source code and
object code form.  This patent license shall apply to the combination of the
Contribution and the Program if, at the time the Contribution is added by the
Contributor, such addition of the Contribution causes such combination to be
covered by the Licensed Patents. The patent license shall not apply to any
other combinations which include the Contribution. No hardware per se is
licensed hereunder.

c) Recipient understands that although each Contributor grants the licenses
to its Contributions set forth herein, no assurances are provided by any
Contributor that the Program does not infringe the patent or other
intellectual property rights of any other entity. Each Contributor disclaims
any liability to Recipient for claims brought by any other entity based on
infringement of intellectual property rights or otherwise. As a condition to
exercising the rights and licenses granted hereunder, each Recipient hereby
assumes sole responsibility to secure any other intellectual property rights
needed, if any. For example, if a third party patent license is required to
allow Recipient to distribute the Program, it is Recipient's responsibility
to acquire that license before distributing the Program.

d) Each Contributor represents that to its knowledge it has sufficient
copyright rights in its Contribution, if any, to grant the copyright license
set forth in this Agreement.

3. REQUIREMENTS

A Contributor may choose to distribute the Program in object code form under
its own license agreement, provided that:

a) it complies with the terms and conditions of this Agreement; and

b) its license agreement:

i) effectively disclaims on behalf of all Contributors all warranties and
conditions, express and implied, including warranties or conditions of title
and non-infringement, and implied warranties or conditions of merchantability
and fitness for a particular purpose;

ii) effectively excludes on behalf of all Contributors all liability for
damages, including direct, indirect, special, incidental and consequential
damages, such as lost profits;

iii) states that any provisions which differ from this Agreement are offered
by that Contributor alone and not by any other party; and

iv) states that source code for the Program is available from such
Contributor, and informs licensees how to obtain it in a reasonable manner on
or through a medium customarily used for software exchange.

When the Program is made available in source code form:

a) it must be made available under this Agreement; and

b) a copy of this Agreement must be included with each copy of the Program.

Contributors may not remove or alter any copyright notices contained within
the Program.

Each Contributor must identify itself as the originator of its Contribution,
if any, in a manner that reasonably allows subsequent Recipients to identify
the originator of the Contribution.

4. COMMERCIAL DISTRIBUTION

Commercial distributors of software may accept certain responsibilities with
respect to end users, business partners and the like. While this license is
intended to facilitate the commercial use of the Program, the Contributor who
includes the Program in a commercial product offering should do so in a
manner which does not create potential liability for other Contributors.
Therefore, if a Contributor includes the Program in a commercial product
offering, such Contributor ("Commercial Contributor") hereby agrees to defend
and indemnify every other Contributor ("Indemnified Contributor") against any
losses, damages and costs (collectively "Losses") arising from claims,
lawsuits and other legal actions brought by a third party against the
Indemnified Contributor to the extent caused by the acts or omissions of such
Commercial Contributor in connection with its distribution of the Program in
a commercial product offering.  The obligations in this section do not apply
to any claims or Losses relating to any actual or alleged intellectual
property infringement. In order to qualify, an Indemnified Contributor must:
a) promptly notify the Commercial Contributor in writing of such claim, and
b) allow the Commercial Contributor to control, and cooperate with the
Commercial Contributor in, the defense and any related settlement
negotiations. The Indemnified Contributor may participate in any such claim
at its own expense.

For example, a Contributor might include the Program in a commercial product
offering, Product X. That Contributor is then a Commercial Contributor. If
that Commercial Contributor then makes performance claims, or offers
warranties related to Product X, those performance claims and warranties are
such Commercial Contributor's responsibility alone. Under this section, the
Commercial Contributor would have to defend claims against the other
Contributors related to those performance claims and warranties, and if a
court requires any other Contributor to pay any damages as a result, the
Commercial Contributor must pay those damages.

5. NO WARRANTY

EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, THE PROGRAM IS PROVIDED ON
AN "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, EITHER
EXPRESS OR IMPLIED INCLUDING, WITHOUT LIMITATION, ANY WARRANTIES OR
CONDITIONS OF TITLE, NON-INFRINGEMENT, MERCHANTABILITY OR FITNESS FOR A
PARTICULAR PURPOSE. Each Recipient is solely responsible for determining the
appropriateness of using and distributing the Program and assumes all risks
associated with its exercise of rights under this Agreement , including but
not limited to the risks and costs of program errors, compliance with
applicable laws, damage to or loss of data, programs or equipment, and
unavailability or interruption of operations.

6. DISCLAIMER OF LIABILITY

EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, NEITHER RECIPIENT NOR ANY
CONTRIBUTORS SHALL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING WITHOUT LIMITATION
LOST PROFITS), HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OR DISTRIBUTION OF THE PROGRAM OR THE
EXERCISE OF ANY RIGHTS GRANTED HEREUNDER, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGES.

7. GENERAL

If any provision of this Agreement is invalid or unenforceable under
applicable law, it shall not affect the validity or enforceability of the
remainder of the terms of this Agreement, and without further action by the
parties hereto, such provision shall be reformed to the minimum extent
necessary to make such provision valid and enforceable.

If Recipient institutes patent litigation against any entity (including a
cross-claim or counterclaim in a lawsuit) alleging that the Program itself
(excluding combinations of the Program with other software or hardware)
infringes such Recipient's patent(s), then such Recipient's rights granted
under Section 2(b) shall terminate as of the date such litigation is filed.

All Recipient's rights under this Agreement shall terminate if it fails to
comply with any of the material terms or conditions of this Agreement and
does not cure such failure in a reasonable period of time after becoming
aware of such noncompliance. If all Recipient's rights under this Agreement
terminate, Recipient agrees to cease use and distribution of the Program as
soon as reasonably practicable. However, Recipient's obligations under this
Agreement and any licenses granted by Recipient relating to the Program shall
continue and survive.

Everyone is permitted to copy and distribute copies of this Agreement, but in
order to avoid inconsistency the Agreement is copyrighted and may only be
modified in the following manner. The Agreement Steward reserves the right to
publish new versions (including revisions) of this Agreement from time to
time. No one other than the Agreement Steward has the right to modify this
Agreement. The Eclipse Foundation is the initial Agreement Steward. The
Eclipse Foundation may assign the responsibility to serve as the Agreement
Steward to a suitable separate entity. Each new version of the Agreement will
be given a distinguishing version number. The Program (including
Contributions) may always be distributed subject to the version of the
Agreement under which it was received. In addition, after a new version of
the Agreement is published, Contributor may elect to distribute the Program
(including its Contributions) under the new version. Except as expressly
stated in Sections 2(a) and 2(b) above, Recipient receives no rights or
licenses to the intellectual property of any Contributor under this
Agreement, whether expressly, by implication, estoppel or otherwise. All
rights in the Program not expressly granted under this Agreement are
reserved.

This Agreement is governed by the laws of the State of New York and the
intellectual property laws of the United States of America. No party to this
Agreement will bring a legal action under this Agreement more than one year
after the cause of action arose. Each party waives its rights to a jury trial
in any resulting litigation.

```

`/Users/ovistoica/workspace/voice-fn/CHANGELOG.md`:

```md
# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Changed
- Add a new arity to `make-widget-async` to provide a different widget shape.

## [0.1.1] - 2024-12-24
### Changed
- Documentation on how to make the widgets.

### Removed
- `make-widget-sync` - we're all async, all the time.

### Fixed
- Fixed widget maker to keep working when daylight savings switches over.

## 0.1.0 - 2024-12-24
### Added
- Files from the new template.
- Widget maker public API - `make-widget-sync`.

[Unreleased]: https://github.com/ovistoica/voice-fn/compare/0.1.1...HEAD
[0.1.1]: https://github.com/ovistoica/voice-fn/compare/0.1.0...0.1.1

```

`/Users/ovistoica/workspace/voice-fn/TODO.org`:

```org
#+TITLE: Tasks to do for voice-fn

* DONE Add schema validation with defaults
CLOSED: [2025-01-03 Fri 11:01]
:LOGBOOK:
CLOCK: [2025-01-03 Fri 07:51]--[2025-01-03 Fri 08:16] =>  0:25
CLOCK: [2025-01-03 Fri 07:06]--[2025-01-03 Fri 07:31] =>  0:25
CLOCK: [2025-01-03 Fri 06:35]--[2025-01-03 Fri 07:00] =>  0:25
:END:

* TODO Add VAD events from deepgram
:LOGBOOK:
CLOCK: [2025-01-03 Fri 11:01]
:END:

* TODO Add support for first message

* TODO Add support for [[https://github.com/fixie-ai/ultravox][ultravox]]

* TODO Add support for google gemini

* TODO Add support for telnyx transport

* TODO Add support for xAI text LLM

* TODO Add support for openai realtime API

* TODO Research webrtc support

```

`/Users/ovistoica/workspace/voice-fn/README.md`:

```md
# voice-fn - WIP

A Clojure library for building real-time voice-enabled AI applications. voice-fn handles the orchestration of speech recognition, audio processing, and AI service integration with the elegance of functional programming.

## Features

- Real-time speech recognition with configurable backends
- Clean functional interface for audio stream processing
- Seamless integration with popular AI models and services
- Built-in support for conversation state management
- Efficient audio processing with core.async channels
- Hot-reloadable pipeline components
- Extensible architecture with composable transforms

## Why voice-fn?

voice-fn brings Clojure's functional elegance to voice AI applications. Rather than wrestling with complex state management and imperative audio processing, voice-fn lets you create AI pipelines in a declarative way.

```clojure
(ns example
  (:require
   [voice-fn.pipeline :as pipeline]
   [voice-fn.secrets :refer [secret]]))

(def async-echo-pipeline
  {:pipeline/config {:audio-in/sample-rate 8000
                     :audio-in/encoding :ulaw
                     :audio-in/channels 1
                     :audio-in/sample-size-bits 8
                     :audio-out/sample-rate 8000
                     :audio-out/bitrate 64000
                     :audio-out/sample-size-bits 8
                     :audio-out/channels 1
                     :pipeline/language :ro}
   :pipeline/processors [;;transport in
                         {:processor/type :transport/async-input
                          :processor/accepted-frames #{:system/start :system/stop}
                          :processor/generates-frames #{:audio/raw-input}}
                         ;; transcription
                         {:processor/type :transcription/deepgram
                          :processor/accepted-frames #{:system/start :system/stop :audio/raw-input}
                          :processor/generates-frames #{:text/input}
                          :processor/config {:transcription/api-key (secret [:deepgram :api-key])
                                             :transcription/interim-results? false
                                             :transcription/punctuate? false
                                             :transcription/model :nova-2}}
                        ;; text to speech
                        {:processor/type :tts/openai
                         :processor/accepted-frames #{:text/input}
                         :processor/generates-frames #{:audio/output}
                         :processor/config {:openai/api-key (secret [:openai :api-key])}}
                        ;; transport out
                         {:processor/type :transport/async-output
                          :processor/accepted-frames #{:audio/output :system/stop}
                          :generates/frames #{}}]})

(def p (pipeline/create-pipeline async-echo-pipeline))
(pipeline/start-pipeline! p) ;;starting pipeline
(pipeline/stop-pipeline! p) ;;stopping
```

To add a new processor simply tell voice-fn how to run `process-frame` for that new processor. Example of a frame logger for text-input frames
```clojure
(ns my-logger-processor
  (:require
   [voice-fn.pipeline :as pipeline]))

(defmethod pipeline/process-frame :log/text-input
  [_ _ _ frame]
  (t/log! {:level :info
           :id :log/text-input} ["Frame" (:frame/data frame)]))


;; and now you can use it in the pipeline
(def async-echo-pipeline
  {:pipeline/config {...}
   :pipeline/processors [...
                         {:processor/type :transcription/deepgram
                          :processor/accepted-frames #{:system/start :system/stop :audio/raw-input}
                          :processor/generates-frames #{:text/input}
                          :processor/config {:transcription/api-key (secret [:deepgram :api-key])
                                             :transcription/interim-results? false
                                             :transcription/punctuate? false
                                             :transcription/model :nova-2}}
                         {:processor/type :log/text-input
                          :processor/accepted-frames #{:text/input}
                          :processor/generates-frames #{}
                          :processor/config {}}
                          ...]})
```

## Status

WIP - Early stage. See examples for what can be achieved for now

## Documentation

WIP - TODO

See [docs/](docs/) for full documentation, including:
- Getting Started Guide
- Architecture Overview
- API Reference
- Examples
- Contributing Guidelines

## License

MIT

---

voice-fn - Functional voice AI, powered by Clojure.

```

`/Users/ovistoica/workspace/voice-fn/examples/deps.edn`:

```edn
{:paths ["src"]
 :deps {org.clojure/clojure {:mvn/version "1.12.0"}
        org.clojure/data.xml {:mvn/version "0.0.8"}
        ring/ring-jetty-adapter {:mvn/version "1.13.0"}
        com.shipclojure/voice-fn {:local/root "../core"}
        metosin/reitit {:mvn/version "0.7.2"}
        hato/hato {:mvn/version "1.0.0"}}
 :aliases {:cider-clj {:extra-deps {cider/cider-nrepl {:mvn/version "0.50.3"}}
                       :main-opts ["-m" "nrepl.cmdline" "--middleware" "[cider.nrepl/cider-middleware]"]}}}

```

`/Users/ovistoica/workspace/voice-fn/examples/src/deps.edn`:

```edn
{:paths ["src" "resources"]
 :deps {org.clojure/clojure {:mvn/version "1.12.0"}
        com.taoensso/telemere {:mvn/version "1.0.0-RC1"}
        org.clojure/core.async {:mvn/version "1.7.701"}
        metosin/jsonista {:mvn/version "0.3.8"}
        ring/ring-core {:mvn/version "1.13.0"}
        hato/hato {:mvn/version "1.0.0"}}
 :aliases
 {:build {:deps {io.github.clojure/tools.build {:mvn/version "0.9.4"}
                 slipset/deps-deploy {:mvn/version "0.2.1"}}
          :ns-default build}
  :dev {:extra-deps {djblue/portal {:mvn/version "0.58.5"}}}
  :cider-clj {:extra-deps {cider/cider-nrepl {:mvn/version "0.50.3"}}
              :main-opts ["-m" "nrepl.cmdline" "--middleware" "[cider.nrepl/cider-middleware]"]}
  :test {:extra-paths ["test"]
         :extra-deps {org.clojure/test.check {:mvn/version "1.1.1"}
                      midje/midje {:mvn/version "1.10.10"}
                      io.github.cognitect-labs/test-runner
                      {:git/tag "v0.5.1" :git/sha "dfb30dd"}}}}}

```

`/Users/ovistoica/workspace/voice-fn/examples/src/voice_fn_examples/twilio_websocket.clj`:

```clj
(ns voice-fn-examples.twilio-websocket
  (:require
   [clojure.core.async :as a]
   [clojure.data.xml :as xml]
   [muuntaja.core :as m]
   [reitit.core]
   [reitit.dev.pretty :as pretty]
   [reitit.ring :as ring]
   [reitit.ring.coercion :as coercion]
   [reitit.ring.middleware.exception :as exception]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.ring.middleware.parameters :as parameters]
   [ring.adapter.jetty :as jetty]
   [ring.util.response :as r]
   [ring.websocket :as ws]
   [taoensso.telemere :as t]
   [voice-fn.core]
   [voice-fn.pipeline :as vpipe]
   [voice-fn.processors.elevenlabs]
   [voice-fn.processors.llm-context-aggregator]
   [voice-fn.processors.llm-sentence-assembler]
   [voice-fn.secrets :refer [secret]]
   [voice-fn.transport.twilio]))

(defn emit-xml-str
  [xml-data]
  (-> xml-data
      (xml/sexp-as-element)
      (xml/emit-str)))

(defn header
  [req header-name]
  (get (:headers req) header-name))

(defn host
  [req]
  (header req "host"))

(defn xml-response
  [body]
  (-> (r/response body)
      (r/content-type "text/xml")))

(defn twilio-inbound-handler
  "Handler to direct the incoming call to stream audio to our websocket handler"
  [req]
  (let [h (host req)
        ws-url (str "wss://" h "/ws")]
    ;; https://www.twilio.com/docs/voice/twiml/connect
    (xml-response
      (emit-xml-str [:Response
                     [:Connect
                      [:Stream {:url ws-url}]]]))))

(defn create-twilio-ai-pipeline
  [in out]
  {:pipeline/config {:audio-in/sample-rate 8000
                     :audio-in/encoding :ulaw
                     :audio-in/channels 1
                     :audio-in/sample-size-bits 8
                     :audio-out/sample-rate 8000
                     :audio-out/encoding :ulaw
                     :audio-out/sample-size-bits 8
                     :audio-out/channels 1
                     :pipeline/language :ro
                     :llm/context [{:role "system" :content  "Ești un agent vocal care funcționează prin telefon. Răspunde doar în limba română și fii succint. Inputul pe care îl primești vine dintr-un sistem de speech to text (transcription) care nu este intotdeauna eficient și poate trimite text neclar. Cere clarificări când nu ești sigur pe ce a spus omul."}]
                     :transport/in-ch in
                     :transport/out-ch out}
   :pipeline/processors [{:processor/type :transport/twilio-input
                          :processor/accepted-frames #{:system/start :system/stop}
                          :processor/generates-frames #{:audio/raw-input}}
                         {:processor/type :transcription/deepgram
                          :processor/accepted-frames #{:system/start :system/stop :audio/raw-input}
                          :processor/generates-frames #{:text/input}
                          :processor/config {:transcription/api-key (secret [:deepgram :api-key])
                                             :transcription/interim-results? true
                                             :transcription/punctuate? false
                                             :transcription/vad-events? true
                                             :transcription/smart-format? true
                                             :transcription/model :nova-2}}
                         {:processor/type :llm/context-aggregator
                          :processor/accepted-frames #{:llm/output-text-sentence :text/input}
                          :processor/generates-frames #{:llm/user-context-added}}
                         {:processor/type :llm/openai
                          :processor/accepted-frames #{:llm/user-context-added}
                          :processor/generates-frames #{:llm/output-text-chunk}
                          :processor/config {:llm/model "gpt-4o-mini"
                                             :openai/api-key (secret [:openai :new-api-sk])}}
                         {:processor/type :log/text-input
                          :processor/accepted-frames #{:text/input}
                          :processor/generates-frames #{}
                          :processor/config {}}
                         {:processor/type :llm/sentence-assembler
                          :processor/accepted-frames #{:system/stop :llm/output-text-chunk}
                          :processor/generates-frames #{:llm/output-text-sentence}
                          :processor/config {:sentence/end-matcher #"[.?!;:]"}}
                         {:processor/type :tts/elevenlabs
                          :processor/accepted-frames #{:system/stop :system/start :llm/output-text-sentence}
                          :processor/generates-frames #{:audio/output :elevenlabs/audio-chunk}
                          :processor/config {:elevenlabs/api-key (secret [:elevenlabs :api-key])
                                             :elevenlabs/model-id "eleven_flash_v2_5"
                                             :elevenlabs/voice-id "7sJPxFeMXAVWZloGIqg2"
                                             :voice/stability 0.5
                                             :voice/similarity-boost 0.8
                                             :voice/use-speaker-boost? true}}
                         {:processor/type :elevenlabs/audio-assembler
                          :processor/accepted-frames #{:elevenlabs/audio-chunk}
                          :processor/generates-frames #{:audio/output}}
                         {:processor/type :transport/async-output
                          :processor/accepted-frames #{:audio/output :system/stop}
                          :generates/frames #{}}]})

;; Using ring websocket protocols to setup a websocket server
(defn twilio-ws-handler
  [req]
  (assert (ws/upgrade-request? req))
  (let [in (a/chan 1024)
        out (a/chan 1024)
        pipeline (vpipe/create-pipeline (create-twilio-ai-pipeline in out))
        start-pipeline (fn [socket]
                         ;; listen on the output channel we provided to send
                         ;; that audio back to twilio
                         (a/go-loop []
                           (when-let [output (a/<! out)]
                             (t/log! {:id :twilio-output
                                      :level :debug} ["Sending twilio output" output])
                             (ws/send socket output)
                             (recur)))
                         (vpipe/start-pipeline! pipeline))]
    {::ws/listener
     {:on-open (fn on-open [socket]
                 (start-pipeline socket)

                 nil)
      :on-message (fn on-text [_ws payload]
                    (a/put! in payload))
      :on-close (fn on-close [_ws _status-code _reason]
                  (vpipe/stop-pipeline! pipeline))
      :on-error (fn on-error [ws error]
                  (prn error)
                  (t/log! :debug error))
      :on-ping (fn on-ping [ws payload]
                 (ws/send ws payload))}}))

(def routes
  [["/inbound-call" {:summary "Webhook where a call is made"
                     :post {:handler twilio-inbound-handler}}]
   ["/ws" {:summary "Websocket endpoint to receive a twilio call"
           :get {:handler twilio-ws-handler}}]])

(def app
  (ring/ring-handler
    (ring/router
      routes
      {:exception pretty/exception
       :data {:muuntaja m/instance
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
                           coercion/coerce-request-middleware]}})
    (ring/create-default-handler)))

(defn start [& {:keys [port] :or {port 3000}}]
  (println (str "server running in port " port))
  (jetty/run-jetty #'app {:port port, :join? false}))

(comment

  (def server (start :port 3000))
  (.stop server)

  ,)

```

`/Users/ovistoica/workspace/voice-fn/doc/intro.md`:

```md
# Introduction to voice-fn

TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)

```

`/Users/ovistoica/workspace/voice-fn/architecture.org`:

```org
:PROPERTIES:
:GPTEL_MODEL: claude-3-5-sonnet-20241022
:GPTEL_BACKEND: Claude
:GPTEL_SYSTEM: You are a Clojure programming expert with deep knowledge of functional programming paradigms, Structure and Interpretation of Computer Programs (SICP), and extensive experience with Clojure's concurrency patterns. Your approach to problem-solving prioritizes data and its transformation, following Rich Hickey's philosophy of "data first, not methods first."\n\nCore Competencies:\n\n1. Functional Programming Expertise\n- You understand and can explain pure functions, immutability, and referential transparency\n- You can demonstrate the benefits of persistent data structures\n- You're well-versed in higher-order functions, function composition, and point-free style\n- You understand the trade-offs between eager and lazy evaluation\n- You can explain and implement functional design patterns\n\n2. SICP Mastery\n- You can explain and implement metacircular evaluators\n- You understand environment model of evaluation\n- You can implement streams and delayed evaluation\n- You're familiar with register machines and compilation\n- You can explain and implement symbolic differentiation\n- You understand and can implement constraint propagation systems\n\n3. Clojure-Specific Knowledge\n- Deep understanding of Clojure's core abstractions: sequences, transducers, protocols\n- Mastery of Clojure's reference types: atoms, refs, agents, vars\n- Expert knowledge of Clojure's concurrent programming models\n- Understanding of Clojure's relationship with the host platform (JVM)\n- Familiarity with ClojureScript and its ecosystem\n\n4. Concurrency Patterns\n- Expert understanding of Software Transactional Memory (STM) using refs\n- Mastery of core.async for CSP-style concurrency\n- Understanding of agent-based concurrency for independent state management\n- Knowledge of Java interop for thread management when necessary\n- Experience with reactive programming patterns\n\n5. Data-First Philosophy\n- You always start by designing the data structure before writing functions\n- You understand and can implement EAV (Entity-Attribute-Value) patterns\n- You're familiar with Datomic and its approach to data management\n- You understand the power of data literals and EDN\n- You can explain and implement data-driven programming patterns\n\nApproach to Problem-Solving:\n\n1. When presented with a problem, you:\n   - First analyze and design the data structures needed\n   - Consider immutability and persistence requirements\n   - Evaluate concurrency needs early in the design process\n   - Think in terms of data transformations rather than objects and methods\n\n2. When reviewing code, you look for:\n   - Proper separation of pure and impure functions\n   - Appropriate use of Clojure's reference types\n   - Efficient use of lazy sequences and transducers\n   - Clear data transformation pipelines\n   - Proper error handling and validation\n\n3. When designing systems, you:\n   - Start with the data model and its evolution over time\n   - Consider the query patterns that will be needed\n   - Plan for concurrent access patterns\n   - Design for composability and reuse through data transformation\n\nBest Practices You Follow:\n\n1. Data Design\n   - Use maps as the primary unit of data\n   - Prefer sets for unique collections\n   - Use vectors for ordered sequences\n   - Use keywords as keys for better performance\n   - Consider spec for data validation\n\n2. Function Design\n   - Write small, focused functions\n   - Use threading macros for clarity\n   - Leverage higher-order functions\n   - Use destructuring for clean parameter handling\n   - Document functions with clear specs\n\n3. Concurrency Handling\n   - Use refs for coordinated state changes\n   - Use atoms for independent state\n   - Use agents for asynchronous updates\n   - Use core.async for complex coordination\n   - Always consider transaction boundaries\n\n4. Error Handling\n   - Use ex-info for structured errors\n   - Leverage spec for validation\n   - Use proper exception handling patterns\n   - Consider retry strategies for concurrent operations\n\nWhen responding to questions:\n1. Always start by examining the data structures involved\n2. Consider concurrency implications early\n3. Suggest the simplest solution that solves the problem\n4. Provide examples using real-world scenarios\n5. Explain the trade-offs of different approaches\n6. Reference relevant sections of SICP when applicable\n7. Share insights from Clojure's core principles\n\nWhen writing code:\n1. Prioritize clarity over cleverness\n2. Use proper formatting and indentation\n3. Include relevant docstrings and comments\n4. Demonstrate idiomatic Clojure patterns\n5. Show test cases when appropriate\n6. Consider performance implications\n7. Document any assumptions made\n\nYou should be able to discuss and implement:\n- Custom data structures using protocols\n- Advanced macro systems\n- Domain-specific languages\n- Clojure's core protocols\n- Integration with Java libraries\n- Performance optimization techniques\n- Testing strategies\n- System architecture patterns\n\nRemember to always approach problems from a data-first perspective, considering the shape and flow of data before implementing functions and processes. Your solutions should embrace Clojure's philosophy of simplicity and power through data transformation.
:GPTEL_BOUNDS: ((10382 . 11028) (11065 . 11506) (11977 . 12681) (12882 . 13619) (13834 . 14372) (14383 . 14716) (14717 . 14911) (17713 . 18078) (18079 . 19845) (20209 . 21647) (21658 . 24816) (24865 . 25752) (25753 . 28085) (28147 . 28270) (28271 . 31697) (41934 . 45355) (45356 . 45425) (45486 . 48142) (48143 . 48564) (48796 . 49027) (49028 . 50961) (51339 . 51494) (51495 . 54150) (54151 . 54609))
:END:

*** Help me think of a good architecture for a project similar to pipecat but written in the clojure ecosystem.

A Clojure framework for building real-time voice-enabled AI applications. voice-fn handles the orchestration of speech recognition, audio processing, and AI service integration with the elegance of functional programming.

## Features

- Real-time speech recognition with configurable backends
- Clean functional interface for audio stream processing
- Seamless integration with popular AI models and services
- Built-in support for conversation state management
- Efficient audio processing with core.async channels
- Hot-reloadable pipeline components
- Extensible architecture with composable transforms


My problems:
- I tried to replicate the same system from pipecat but it doens't map well over clojure way of doing things since it puts things in a very OOP manner.
- I want to use core.async and I was thinking that all processors have an in and a out. They take a frame from in, call =process-frame= on it and immediately or at a later time, puts its resulting frames onto the out channel.
- The way pipecat did it with classes for processors and transports works well but it doesn't feel very clojure with data orientation.

  I welcome any ideas you have. Here is some of my ideas in Clojure so far (I am not very happy with them since I tried to port the OOP model to protocols and records and doesn't hold so well since you need maker functions to simulate async work done in a constructor):

  (ns voice-fn.processors.protocols)

(defprotocol FrameProcessor
  "A frame processor is one of the pieces from the pipeline. Examples:
  - `TranscriptionProcessor`: processes `RawAudioInputFrames` and outputs `TranscriptionOutputFrames`
  - `TextLLMProcessor`: processes `TranscriptionOutputFrames` and outputs `LLMOutputTokenFrames`"
  (process-frame [this frame] "Process a frame of a given type. May output 0, 1 or multiple frames in the pipeline"))

(defprotocol ProcessorLifecycle
  (start! [this] "Start the processor. Optionally push a start frame further in the pipeline")
  (stop! [this] "Stop the processor. Optionally push a stop frame further in the pipeline."))


  (ns voice-fn.processors.deepgram
  (:require
   [hato.websocket :as ws]
   [taoensso.telemere :as t]
   [voice-fn.frames :as frames]
   [voice-fn.processors.protocols :refer [FrameProcessor ProcessorLifecycle stop!]]
   [voice-fn.utils.core :as u])
  (:import
   (java.nio HeapCharBuffer)))

(def ^:private deepgram-url "wss://api.deepgram.com/v1/listen")

(def deepgram-config {:encoding "linear16"
                      :language "ro"
                      :sample_rate 16000
                      :interim_results false
                      :punctuate true
                      :model "nova-2"})

(def deepgram-url-encoding
  (u/append-search-params deepgram-url deepgram-config))

(defn transcript?
  [m]
  (= (:event m) "transcript"))

(defn- transcript
  [m]
  (-> m :channel :alternatives first :transcript))

(defn final-transcription?
  [m]
  (and (transcript? m)
       (= (:type m) "final")))

(defn- make-websocket-config [api-key]
  {:headers {"Authorization" (str "Token " api-key)}
   :on-open (fn [_]
              (t/log! :info "Deepgram websocket connection open"))
   :on-message (fn [_ws ^HeapCharBuffer data _last?]
                 (let [m (u/parse-if-json (str data))
                       trsc (transcript m)]
                   (when (and (final-transcription? m)
                              (not= trsc ""))
                     (t/log! :info ["Transcript" trsc]))))
   :on-error (fn [_ e]
               (t/log! :error ["Error" e]))
   :on-close (fn [_ code reason]
               (t/log! :info ["Deepgram websocket connection closed" "Code:" code "Reason" reason]))})

(defn close-connection-payload
  []
  (u/json-str {:type "CloseStream"}))

(defrecord DeepgramTranscriptionProcessor [conn]
  FrameProcessor
  (process-frame [this frame]
    (when (frames/raw-audio-input-frame? frame)
      (ws/send! (:raw frame) false)))
  ProcessorLifecycle
  (start! [this]
    (t/log! :info "Deepgram transcription started"))
  (stop! [this]
    (t/log! :info "Closing Deepgram connection")
    (ws/send! conn (close-connection-payload))
    (ws/close! conn)))

(defn make-deepgram-transcription-processor
  [api-key]
  (let [conn (ws/websocket deepgram-url-encoding (make-websocket-config api-key))]
    (->DeepgramTranscriptionProcessor conn)))

(comment

  (def processor (make-deepgram-transcription-processor api-key))
  (stop! processor))



Let me guide you towards a more Clojure-centric design. Let's start with some high-level hints:

1. Think about what would happen if you modeled your frames as plain data (maps) instead of types/classes. What benefits might this give you?

2. Consider how core.async's transducers could relate to your frame processing pipeline. How might this align better with Clojure's data transformation philosophy?

3. Instead of protocols and records for processors, what if you thought about them as pure functions that transform data? What would that look like?

Would you like to explore any of these directions further? Which aspect interests you most?

*** Let's explore the model first

Sure! Let's think about modeling frames as data. Here's a hint to get you started:

In Clojure, we often use maps with a =:type= key to distinguish between different kinds of data. How might you represent the different frame types you saw in pipecat (AudioRawFrame, TextFrame, etc.) using just maps?

Try sketching out a few example frames as pure data structures. Don't worry about the processing yet - let's nail down the data model first.

*** The initial dummy way to do it is have a map like this:
{:stt {:provider :elevenlabs
       :config {...}}
 :llm {:provider :llama
       :config}
 ...
 }

 However this model is too static. I want users to be able to put on the pipeline whatever they want. For example, maybe between the TTS and the STT models I want an extra logger that sends telemetry data with the conversation to storage. The correct model is an array with the processors specified in order

Good thinking! You're moving in the right direction with the array of processors.

Here's a hint to push this further: In Clojure, we often represent "configuration as data" where the configuration itself can describe both structure and behavior.

Think about how you might represent a pipeline like this:
#+begin_src clojure
[{:type :stt
  :provider :deepgram
  ...}
 {:type :telemetry
  :store-fn ...}
 {:type :llm
  :provider :openai
  ...}]
#+end_src

What if each processor spec in this vector could also include information about:
- What kinds of frames it can accept
- What kinds of frames it produces
- Any side effects it might have

How might that help with pipeline validation and composition?

*** I don't think we need to specify the type as that doesn't serve any need. Also the pipeline should maintain state. For example, deepgram transcription processor maintains a websocket connection

Ah, now we're getting to an interesting design question! Let me offer a hint:

In Clojure, we often separate the configuration/specification of something from its runtime instance. Consider this pattern:

#+begin_src clojure
;; The specification (pure data)
{:provider :deepgram
 :api-key "..."
 :config {...}}

;; The runtime instance (stateful)
{:chan-in (chan)
 :chan-out (chan)
 :conn (atom nil)  ; websocket connection
 :state (atom {})} ; other state
#+end_src

Think about:
1. How might you transform the static specification into a running instance?
2. Where would the state management code live if not in records/protocols?
3. How could core.async channels fit into this picture?

Would you like to explore any of these aspects?

*** Let's explore the data oriented specification for a simple pipeline:
1. Capture local audio from microphone
2. Send those to a transcription engine (deepgram)
3. Log all of the transcriptions while they come

Good example to start with! Let's think about how to specify this pipeline in a data-oriented way.

Here's a hint: Consider what minimal information you need to specify each step, keeping in mind that we want to separate the "what" from the "how".

Try writing out a data structure that describes this pipeline. Think about:
- How to specify the audio source configuration
- How to connect the components
- What configuration each component needs
- How to specify where the logs should go

Want to take a first pass at this specification?

*** Yes

Okay, let's start sketching. Here's a hint for the structure:

Think about each component in terms of:
1. What it needs to initialize
2. What resources it needs access to
3. What configuration parameters it requires

Try writing a data structure that describes your pipeline. You might start with something like:

#+begin_src clojure
[{:source :microphone
  ...}
 {:transcribe :deepgram
  ...}
 {:log ...}]
#+end_src

Fill in what you think each component needs. Don't worry about getting it perfect - we can refine it together.

*** No, it should be something like this:
#+begin_src clojure
(defn- make-websocket-config [api-key out-chan]
  {:headers {"Authorization" (str "Token " api-key)}
   :on-open (fn [_]
              (t/log! :info "Deepgram websocket connection open"))
   :on-message (fn [_ws ^HeapCharBuffer data _last?]
                 (let [m (u/parse-if-json (str data))
                       trsc (transcript m)]
                   (when (and (final-transcription? m)
                              (not= trsc ""))
                     (a/go (a/>! out-chan (frames/text-input-frame)))))
   :on-error (fn [_ e]
               (t/log! :error ["Error" e]))
   :on-close (fn [_ code reason]
               (t/log! :info ["Deepgram websocket connection closed" "Code:" code "Reason" reason]))})

(defn create-deepgram-transcription-websocket [state config]
  (ws/websocket deepgram-url-encoding (make-websocket-config (:api-key config) (:main-ch @state))))

(defn get-connection! [state config]
  (if-let [conn (get-in [:transcription/deepgram :conn] @state)]
    conn
    (swap! assoc-in [:transcription/deepgram :conn] (create-deepgram-transcription-websocket state config))))

[{:accepted-frames #{:system/start :system/stop :system/config} ;; doesn't need to interrupt on user speech
  :generated-frames #{:audio/raw-input}
  :process-frame-fn (fn [state config frame]
                      (let [running? (atom false)]
                        (condp type frame
                          :system/start (start-capture-audio (:audio-config @state))
                          :system/stop (stop-capture-audio)
                          )))
  ;; meta is used to access processor specific state
  :type :transport/local-audio
  :direction :input}
 {:accepted-frames #{:system/start :system/stop :system/config :audio/raw-input} ;; also doesn't need to interrupt
  :generated-frames #{:text/input}
  :api-key "..."
  :process-frame-fn
  (fn [state config frame]
    (condp type frame
      :system/start (noop)
      :system/stop (let [conn (get-in [:transcription/deepgram :conn] @state)]
                     (close-connection! conn))
      ;; deepgram websocket times out if we don't send data fast, so we create
      ;; the connection only when we have some actual data to process
      :audio/raw-input (let [conn (get-connection! state config)]
                         (ws/send! conn (:raw frame)))

      )
    )
  :type :transcription/deepgram
  }
 {:accepted-frames #{:system/start :system/stop :system/config :text/input} ;; also doesn't need to interrupt
  :generated-frames #{}
  :process-frame-fn
  (fn [state config frame]
    (condp type frame
      :system/start (noop)
      :system/stop (noop)
      :text/input (t/log! :info (:text frame))

      )
    )
  :type :log/text-input
  }
 ]

#+end_src


Ah, now I see where you're going! This is a good start - you're thinking about frames as data flowing through the system and each processor declaring what it can accept and produce. Let me help refine this model.

A few suggestions to make this more idiomatic Clojure:

1. Consider separating the static configuration from the processing logic:

#+begin_src clojure

(def processors
  {:transport/local-audio
   {:accepted-frames #{:system/start :system/stop :system/config}
    :generated-frames #{:audio/raw-input}
    :direction :input}

   :transcription/deepgram
   {:accepted-frames #{:system/start :system/stop :system/config :audio/raw-input}
    :generated-frames #{:text/input}}

   :log/text-input
   {:accepted-frames #{:system/start :system/stop :system/config :text/input}
    :generated-frames #{}}})

(defmulti process-frame
  (fn [processor-type state config frame]
    processor-type))

(defmethod process-frame :transport/local-audio
  [_ state config frame]
  (case (:type frame)
    :system/start (start-capture-audio (:audio-config @state))
    :system/stop (stop-capture-audio)
    ;; ... etc
    ))
#+end_src

2. For pipeline construction, you could have:

#+begin_src clojure
(defn make-pipeline [processors-config]
  (let [state (atom {})
        channels (create-channels processors-config)]
    (doseq [processor processors-config]
      (start-processor! processor state channels))
    {:state state
     :channels channels}))

;; Usage:
(def pipeline-config
  [{:type :transport/local-audio
    :config {...}}
   {:type :transcription/deepgram
    :config {:api-key "..."}}
   {:type :log/text-input}])
#+end_src

3. For handling the channels:

#+begin_src clojure
(defn start-processor! [{:keys [type config]} state channels]
  (let [in-chan (get-in channels [type :in])
        out-chan (get-in channels [type :out])]
    (go-loop []
      (when-let [frame (<! in-chan)]
        (when-let [result (process-frame type state config frame)]
          (>! out-chan result))
        (recur)))))
#+end_src

What do you think about these refinements? Would you like to explore any particular aspect further?

*** It's interesting for sure. I was thinking that it might be better to have one main frame channel where all frames are put and every processor subscribes to the types of frames they care about. Your example in start-processor!, suggests that all processors have an in and an out which doesn't align with this idea. What do you think? We can use mult and tap

Ah, that's a great observation! Using a single shared channel with =mult= and =tap= is indeed more elegant and flexible. It better matches the publish-subscribe pattern and eliminates the need to manually wire up channels between processors.

Here's how we might reshape this:

#+begin_src clojure
(defn create-pipeline [processors-config]
  (let [main-ch (chan 1024)
        main-mult (mult main-ch)
        state (atom {:main-ch main-ch
                    :main-mult main-mult})]

    ;; Start each processor
    (doseq [{:keys [type accepted-frames] :as processor} processors-config]
      (let [processor-ch (chan 1024)
            ;; Tap into main channel, filtering for accepted frame types
            _ (tap main-mult processor-ch (comp accepted-frames :type))]

        ;; Start processor loop
        (go-loop []
          (when-let [frame (<! processor-ch)]
            (when-let [result (process-frame type state processor frame)]
              ;; Put results back on main channel
              (>! main-ch result))
            (recur)))))

    state))
#+end_src

This approach has several benefits:
1. Processors don't need to know about each other
2. New processors can be added dynamically by just tapping into the main channel
3. Frame routing is handled declaratively via the =accepted-frames= set
4. We get automatic filtering of irrelevant frames

Would you like to explore how this might work with a concrete example?

*** Yes

Let's implement your original example (microphone → transcription → logging) using this pattern:

#+begin_src clojure
(ns voice-fn.core
  (:require [clojure.core.async :as a :refer [chan mult tap go-loop >! <!]]))

;; Frame constructors (pure functions)
(defn audio-frame [raw-data]
  {:type :audio/raw-input
   :data raw-data
   :ts (System/currentTimeMillis)})

(defn text-frame [text]
  {:type :text/input
   :text text
   :ts (System/currentTimeMillis)})

;; Processor implementations
(defmethod process-frame :transport/local-audio
  [_ state {:keys [sample-rate channels] :as config} frame]
  (case (:type frame)
    :system/start
    (let [audio-stream (start-audio-capture! sample-rate channels)
          stop-fn (fn [] (stop-audio-capture! audio-stream))]
      ;; Store stop-fn in state for cleanup
      (swap! state assoc-in [:transport/local-audio :stop-fn] stop-fn)
      ;; Start sending audio frames
      (go-loop []
        (when-let [data (<! audio-stream)]
          (>! (:main-ch @state) (audio-frame data))
          (recur))))

    :system/stop
    (when-let [stop-fn (get-in @state [:transport/local-audio :stop-fn])]
      (stop-fn))))

(defmethod process-frame :transcription/deepgram
  [_ state {:keys [api-key] :as config} frame]
  (case (:type frame)
    :system/start
    (let [ws-conn (create-deepgram-websocket!
                   api-key
                   {:on-transcription
                    (fn [text]
                      (a/put! (:main-ch @state) (text-frame text)))})]
      (swap! state assoc-in [:transcription/deepgram :conn] ws-conn))

    :system/stop
    (when-let [conn (get-in @state [:transcription/deepgram :conn])]
      (close-connection! conn))

    :audio/raw-input
    (when-let [conn (get-in @state [:transcription/deepgram :conn])]
      (send-audio! conn (:data frame)))))

(defmethod process-frame :log/text-input
  [_ state config frame]
  (case (:type frame)
    :text/input
    (println "Transcription:" (:text frame))))

;; Pipeline configuration
(def pipeline-config
  [{:type :transport/local-audio
    :accepted-frames #{:system/start :system/stop}
    :config {:sample-rate 16000
            :channels 1}}

   {:type :transcription/deepgram
    :accepted-frames #{:system/start :system/stop :audio/raw-input}
    :config {:api-key "your-api-key-here"}}

   {:type :log/text-input
    :accepted-frames #{:text/input}
    :config {}}])

;; Usage
(defn start-pipeline! []
  (let [pipeline (create-pipeline pipeline-config)
        main-ch (:main-ch @pipeline)]
    ;; Send start signal to all processors
    (a/put! main-ch {:type :system/start})
    pipeline))

(defn stop-pipeline! [pipeline]
  (a/put! (:main-ch @pipeline) {:type :system/stop}))

(comment
  ;; Start the pipeline
  (def p (start-pipeline!))

  ;; Stop it after some time
  (stop-pipeline! p))
#+end_src

Key points about this design:

1. Single shared channel (=main-ch=) that all frames flow through
2. Each processor declares what frame types it accepts via =accepted-frames=
3. State is managed in one place but partitioned by processor type
4. Clean separation between configuration and runtime behavior
5. Easy to ad

*** How would you split this into namespaces?

Here's a suggested namespace organization that follows Clojure's best practices for separation of concerns:

#+begin_src clojure
voice-fn
├── src
│   └── voice_fn
│       ├── core.clj           ;; Main API namespace
│       ├── frames.clj         ;; Frame definitions and constructors
│       ├── pipeline.clj       ;; Pipeline creation and management
│       ├── protocols.clj      ;; Any protocols we might need
│       ├── specs.clj          ;; Specs for validation
│       ├── processors
│       │   ├── audio.clj      ;; Audio capture processor
│       │   ├── transcription.clj ;; Transcription processors
│       │   └── logging.clj    ;; Logging processors
│       └── utils
│           ├── async.clj      ;; core.async utilities
│           └── websocket.clj  ;; WebSocket utilities
#+end_src

Here's how the code might be organized across these namespaces:

#+begin_src clojure

;; src/voice_fn/frames.clj
(ns voice-fn.frames)

(defn audio-frame [raw-data]
  {:type :audio/raw-input
   :data raw-data
   :ts (System/currentTimeMillis)})

(defn text-frame [text]
  {:type :text/input
   :text text
   :ts (System/currentTimeMillis)})

;; src/voice_fn/pipeline.clj
(ns voice-fn.pipeline
  (:require [clojure.core.async :as a]))

(defn create-pipeline [processors-config]
  ;; Pipeline creation logic here
  )

(defmulti process-frame
  (fn [processor-type state config frame]
    processor-type))

;; src/voice_fn/processors/audio.clj
(ns voice-fn.processors.audio
  (:require [voice-fn.pipeline :refer [process-frame]]
            [voice-fn.frames :as frames]))

(defmethod process-frame :transport/local-audio
  [_ state config frame]
  ;; Audio processor implementation
  )

;; src/voice_fn/processors/transcription.clj
(ns voice-fn.processors.transcription
  (:require [voice-fn.pipeline :refer [process-frame]]
            [voice-fn.frames :as frames]
            [voice-fn.utils.websocket :as ws]))

(defmethod process-frame :transcription/deepgram
  [_ state config frame]
  ;; Transcription processor implementation
  )

;; src/voice_fn/core.clj
(ns voice-fn.core
  (:require [voice-fn.pipeline :as pipeline]
            [voice-fn.processors.audio]    ;; Load multimethod implementations
            [voice-fn.processors.transcription]
            [voice-fn.processors.logging]))

(def default-pipeline-config
  [{:type :transport/local-audio
    :accepted-frames #{:system/start :system/stop}
    :config {:sample-rate 16000
            :channels 1}}
   ;; ... other processor configs
   ])

(defn start!
  ([] (start! default-pipeline-config))
  ([config]
   (let [pipeline (pipeline/create-pipeline config)]
     (a/put! (:main-ch @pipeline) {:type :system/start})
     pipeline)))

(defn stop! [pipeline]
  (a/put! (:main-ch @pipeline) {:type :system/stop}))
#+end_src

Some key points about this organization:

1. =core.clj= provides the main public API
2. Each processor type gets its own namespace
3. Frame definitions are centralized in =frames.clj=
4. Pipeline logic is isolated in =pipeline.clj=
5. Utilities are separated into their own namespaces
6. Processor implementations are loaded by requiring their namespaces

Would you like me to expand on any particular aspect of this organization?

*** Can you also write a function for start-audio-capture!

Here's an implementation of audio capture using the Java Sound API, wrapped in a Clojure-friendly way:

#+begin_src clojure

;; src/voice_fn/processors/audio/capture.clj
(ns voice-fn.processors.audio.capture
  (:require [clojure.core.async :as a])
  (:import [javax.sound.sampled
            AudioSystem
            AudioFormat
            DataLine$Info
            TargetDataLine]))

(defn create-audio-format
  "Creates an AudioFormat with given sample rate and channels.
   Default to 16-bit PCM encoding."
  [sample-rate channels]
  (AudioFormat. sample-rate                  ; sample rate
                16                           ; sample size in bits
                channels                     ; channels
                true                         ; signed
                true))                       ; big endian
(defn open-microphone!
  "Opens the microphone with specified format. Returns the TargetDataLine."
  [^AudioFormat format]
  (let [info (DataLine$Info. TargetDataLine format)
        line (AudioSystem/getLine info)]
    (when-not (.isLineSupported info)
      (throw (ex-info "Audio line not supported"
                     {:format format})))
    (.open ^TargetDataLine line format)
    (.start ^TargetDataLine line)
    line))

(defn start-audio-capture!
  "Starts capturing audio from the microphone.
   Returns a channel that will receive byte arrays of audio data.

   Options:
   :sample-rate - The sample rate in Hz (default: 16000)
   :channels - Number of audio channels (default: 1)
   :buffer-size - Size of the buffer in bytes (default: 4096)
   :chan-buf-size - Size of the core.async channel buffer (default: 1024)"
  ([] (start-audio-capture! {}))
  ([{:keys [sample-rate channels buffer-size chan-buf-size]
     :or {sample-rate 16000
          channels 1
          buffer-size 4096
          chan-buf-size 1024}}]
   (let [format (create-audio-format sample-rate channels)
         line (open-microphone! format)
         out-ch (a/chan chan-buf-size)
         buffer (byte-array buffer-size)
         running? (atom true)]

     ;; Start capture loop in a separate thread
     (future
       (try
         (while @running?
           (let [bytes-read (.read ^TargetDataLine line
                                  buffer
                                  0
                                  buffer-size)]
             (when (pos? bytes-read)
               ;; Copy only the bytes that were read
               (let [audio-data (Arrays/copyOfRange buffer 0 bytes-read)]
                 ;; Put data on channel, but don't block if channel is full
                 (a/offer! out-ch audio-data)))))
         (catch Exception e
           (a/put! out-ch {:error e}))
         (finally
           (.stop ^TargetDataLine line)
           (.close ^TargetDataLine line)
           (a/close! out-ch))))

     ;; Return a map with the channel and a stop function
     {:audio-chan out-ch
      :stop! #(reset! running? false)})))

;; Usage example:
(comment
  ;; Start capture
  (def capture (start-audio-capture! {:sample-rate 16000
                                     :channels 1}))

  ;; Process audio data
  (a/go-loop []
    (when-let [data (a/<! (:audio-chan capture))]
      (println "Got" (count data) "bytes")
      (recur)))

  ;; Stop capture
  ((:stop! capture)))
#+end_src

And here's how to integrate it with your processor:

#+begin_src clojure
;; src/voice_fn/processors/audio.clj
(ns voice-fn.processors.audio
  (:require [voice-fn.pipeline :refer [process-frame]]
            [voice-fn.frames :as

*** I've implemented my first demo of this and it works quite well. Here's how it is structurd:

(ns voice-fn.core
  (:require
   [taoensso.telemere :as t]
   [voice-fn.pipeline :as pipeline]
   [voice-fn.secrets :refer [secret]]
   [voice-fn.transport.local.audio]))

(def pipeline
  {:pipeline/config {:audio-in/sample-rate 16000
                     :audio-in/encoding :pcm-signed
                     :audio-in/channels 1
                     :audio-in/file-path "test-voice.wav"
                     :audio-in/sample-size-bits 16 ;; 2 bytes
                     :audio-out/sample-rate 24000
                     :audio-out/bitrate 96000
                     :audio-out/sample-size-bits 16
                     :audio-out/channels 1
                     :pipeline/language :ro}
   :pipeline/processors [{:processor/type :transport/local-audio
                          :processor/accepted-frames #{:system/start :system/stop}
                          :processor/generates-frames #{:audio/raw-input}}
                         {:processor/type :transcription/deepgram
                          :processor/accepted-frames #{:system/start :system/stop :audio/raw-input}
                          :processor/generates-frames #{:text/input}
                          :processor/config {:transcription/api-key (secret [:deepgram :api-key])
                                             :transcription/interim-results? false
                                             :transcription/punctuate? false
                                             :transcription/model :nova-2}}
                         {:processor/type :log/text-input
                          :processor/accepted-frames #{:text/input}
                          :processor/config {}}]})

(defmethod pipeline/process-frame :log/text-input
  [_ _ _ frame]
  (t/log! {:level :info
           :id :log/text-input} ["Frame" (:data frame)]))

(t/set-min-level! :debug)

(comment
  (def p (pipeline/create-pipeline pipeline))

  (:pipeline/processors @p)

  (pipeline/start-pipeline! p)
  (pipeline/stop-pipeline! p)

  ,)


  (ns voice-fn.transport.local.audio
  (:require
   [clojure.core.async :as a]
   [clojure.java.io :as io]
   [taoensso.telemere :as t]
   [uncomplicate.clojure-sound.core :refer [open! read! start!]]
   [uncomplicate.clojure-sound.sampled :refer [audio-format line line-info]]
   [voice-fn.frames :as frames]
   [voice-fn.pipeline :refer [close-processor! process-frame]])
  (:import
   (java.util Arrays)
   (javax.sound.sampled AudioFormat AudioSystem DataLine$Info TargetDataLine)))

(defn- calculate-chunk-size
  "Calculate bytes for ms miliseconds of audio based on format"
  [ms ^AudioFormat audio-format]
  (let [frame-size (.getFrameSize audio-format)
        frame-rate (.getFrameRate audio-format)]
    (* frame-size (int (/ (* frame-rate ms) 1000)))))

(defn start-audio-capture-file!
  "Reads from WAV file in 20ms chunks.
   Returns a channel that will receive byte arrays of audio data."
  [{:audio-in/keys [file-path]
    :or {file-path "input.wav"}}]
  (let [audio-file (io/resource file-path)
        audio-stream (AudioSystem/getAudioInputStream audio-file)
        audio-format (.getFormat audio-stream)
        chunk-size (calculate-chunk-size 20 audio-format)
        buffer (byte-array chunk-size)
        out-ch (a/chan 1024)
        running? (atom true)]

    (future
      (try
        (while (and @running?
                    (pos? (.read audio-stream buffer 0 chunk-size)))
          (let [audio-data (Arrays/copyOf buffer chunk-size)]
            (a/offer! out-ch audio-data)))
        (catch Exception e
          (a/put! out-ch {:error e}))
        (finally
          (.close audio-stream)
          (a/close! out-ch))))

    {:audio-chan out-ch
     :stop-fn #(do (reset! running? false)
                   (a/close! out-ch))}))

(defn line-supported?
  [^DataLine$Info info]
  (AudioSystem/isLineSupported info))

(defn open-microphone!
  "Opens the microphone with specified format. Returns the TargetDataLine."
  [^AudioFormat format]
  (let [info (line-info :target format)
        line (line info)]
    (when-not (line-supported? info)
      (throw (ex-info "Audio line not supported"
                      {:format format})))
    (open! line format)
    (start! line)
    line))

(defn- frame-buffer-size
  "Get read buffer size based on the sample rate for input"
  [sample-rate]
  (* 2 (/ sample-rate 100)))

(defn start-audio-capture!
  "Starts capturing audio from the microphone.
   Returns a channel that will receive byte arrays of audio data.

   Options:
   :sample-rate - The sample rate in Hz (default: 16000)
   :channels - Number of audio channels (default: 1)"
  ([] (start-audio-capture! {}))
  ([{:audio-in/keys [sample-rate sample-size-bits channels]
     :or {sample-rate 16000
          channels 1
          sample-size-bits 16}}]
   (let [buffer-size (frame-buffer-size sample-rate)
         af (audio-format sample-rate sample-size-bits channels)
         line (open-microphone! af)
         out-ch (a/chan 1024)
         buffer (byte-array buffer-size)
         running? (atom true)]

     ;; Start capture loop in a separate thread
     (future
       (try
         (while @running?
           (let [bytes-read (read! line buffer 0 buffer-size)]
             (when (pos? bytes-read)
               ;; Copy only the bytes that were read
               (let [audio-data (Arrays/copyOfRange buffer 0 bytes-read)]
                 ;; Put data on channel, but don't block if channel is full
                 (a/offer! out-ch audio-data)))))
         (catch Exception e
           (a/put! out-ch {:error e}))
         (finally
           (.stop ^TargetDataLine line)
           (.close ^TargetDataLine line)
           (a/close! out-ch))))

     ;; Return a map with the channel and a stop function
     {:audio-chan out-ch
      :stop-fn #(do (a/close! out-ch)
                    (reset! running? false))})))

(defmethod process-frame :transport/local-audio
  [processor-type pipeline _ frame]
  (case (:frame/type frame)
    :system/start
    (do
      (t/log! :debug "Starting audio capture")
      (let [{:keys [audio-chan stop-fn]} (start-audio-capture-file! (:pipeline/config @pipeline))]
        ;; Store stop-fn in state for cleanup
        (swap! pipeline assoc-in [:transport/local-audio :stop-fn] stop-fn)
        ;; Start sending audio frames
        (a/go-loop []
          (when-let [data (a/<! audio-chan)]
            (a/>! (:pipeline/main-ch @pipeline) (frames/audio-input-frame data))
            (recur)))))

    :system/stop
    (do
      (t/log! :debug "Stopping audio capture")
      (when-let [stop-fn (get-in @pipeline [:transport/local-audio :stop-fn])]
        (stop-fn)))
    (close-processor! pipeline processor-type))
  nil)


  Here's my next issue:

  Other types of transports, like telephony or webrtc, involves opening an API endpoint like a webhook or a websocket to receive the stream of audio for input. How can we set up this during the pipeline? What are your suggestions? Here is how pipecat does it:

  import os
import sys

from dotenv import load_dotenv
from loguru import logger

from pipecat.audio.vad.silero import SileroVADAnalyzer
from pipecat.frames.frames import EndFrame, LLMMessagesFrame
from pipecat.pipeline.pipeline import Pipeline
from pipecat.pipeline.runner import PipelineRunner
from pipecat.pipeline.task import PipelineParams, PipelineTask
from pipecat.processors.aggregators.openai_llm_context import OpenAILLMContext
from pipecat.serializers.twilio import TwilioFrameSerializer
from pipecat.services.cartesia import CartesiaTTSService
from pipecat.services.deepgram import DeepgramSTTService
from pipecat.services.openai import OpenAILLMService
from pipecat.transports.network.fastapi_websocket import (
    FastAPIWebsocketParams,
    FastAPIWebsocketTransport,
)

load_dotenv(override=True)

logger.remove(0)
logger.add(sys.stderr, level="DEBUG")


async def run_bot(websocket_client, stream_sid):
    transport = FastAPIWebsocketTransport(
        websocket=websocket_client,
        params=FastAPIWebsocketParams(
            audio_out_enabled=True,
            add_wav_header=False,
            vad_enabled=True,
            vad_analyzer=SileroVADAnalyzer(),
            vad_audio_passthrough=True,
            serializer=TwilioFrameSerializer(stream_sid),
        ),
    )

    llm = OpenAILLMService(api_key=os.getenv("OPENAI_API_KEY"), model="gpt-4o")

    stt = DeepgramSTTService(api_key=os.getenv("DEEPGRAM_API_KEY"))

    tts = CartesiaTTSService(
        api_key=os.getenv("CARTESIA_API_KEY"),
        voice_id="79a125e8-cd45-4c13-8a67-188112f4dd22",  # British Lady
    )

    messages = [
        {
            "role": "system",
            "content": "You are a helpful LLM in an audio call. Your goal is to demonstrate your capabilities in a succinct way. Your output will be converted to audio so don't include special characters in your answers. Respond to what the user said in a creative and helpful way.",
        },
    ]

    context = OpenAILLMContext(messages)
    context_aggregator = llm.create_context_aggregator(context)

    pipeline = Pipeline(
        [
            transport.input(),  # Websocket input from client
            stt,  # Speech-To-Text
            context_aggregator.user(),
            llm,  # LLM
            tts,  # Text-To-Speech
            transport.output(),  # Websocket output to client
            context_aggregator.assistant(),
        ]
    )

    task = PipelineTask(pipeline, params=PipelineParams(allow_interruptions=True))

    @transport.event_handler("on_client_connected")
    async def on_client_connected(transport, client):
        # Kick off the conversation.
        messages.append({"role": "system", "content": "Please introduce yourself to the user."})
        await task.queue_frames([LLMMessagesFrame(messages)])

    @transport.event_handler("on_client_disconnected")
    async def on_client_disconnected(transport, client):
        await task.queue_frames([EndFrame()])

    runner = PipelineRunner(handle_sigint=False)

    await runner.run(task)


    So I can see here that they instantiate the websocket connection for input transport beforehand



For handling WebSocket/HTTP endpoints in your Clojure pipeline, I'd suggest using Ring/HTTP Kit or Aleph for the server component. Here's how we could structure this:

#+begin_src clojure
;; src/voice_fn/transport/websocket.clj
(ns voice-fn.transport.websocket
  (:require
    [org.httpkit.server :as http]
    [compojure.core :refer [GET defroutes]]
    [voice-fn.pipeline :refer [process-frame]]
    [clojure.core.async :as a]))

(defn websocket-handler [pipeline req]
  (http/with-channel req channel
    (let [client-id (random-uuid)]
      ;; Store WS connection in pipeline state
      (swap! pipeline assoc-in [:transport/websocket :connections client-id] channel)

      ;; Handle incoming messages
      (http/on-receive channel
        (fn [data]
          (a/put! (:pipeline/main-ch @pipeline)
                  (frames/audio-input-frame data))))

      ;; Handle client disconnect
      (http/on-close channel
        (fn [status]
          (swap! pipeline update-in [:transport/websocket :connections] dissoc client-id))))))

(defroutes websocket-routes
  (GET "/ws" [] #(websocket-handler pipeline %)))

(defmethod process-frame :transport/websocket
  [processor-type pipeline config frame]
  (case (:frame/type frame)
    :system/start
    (let [port (get-in config [:processor/config :port] 8080)
          server (http/run-server websocket-routes {:port port})]
      (swap! pipeline assoc-in [:transport/websocket :server] server))

    :system/stop
    (when-let [server (get-in @pipeline [:transport/websocket :server])]
      (server)) ; Stop the server

    :audio/output
    (doseq [[_ conn] (get-in @pipeline [:transport/websocket :connections])]
      (http/send! conn (:data frame)))))

;; Usage in pipeline config:
(def pipeline
  {:pipeline/config {...}
   :pipeline/processors
   [{:processor/type :transport/websocket
     :processor/accepted-frames #{:system/start :system/stop :audio/output}
     :processor/generates-frames #{:audio/raw-input}
     :processor/config {:port 8080}}
    ...]})
#+end_src

Key points:

1. The WebSocket transport is started when it receives =:system/start=
2. It maintains a map of active connections in the pipeline state
3. Incoming messages are put onto the main channel as audio frames
4. Outgoing audio can be sent to all connected clients
5. The server is cleanly shut down on =:system/stop=

For telephony, you might have something similar:

#+begin_src clojure
;; src/voice_fn/transport/telephony.clj
(ns voice-fn.transport.telephony
  (:require
    [ring.adapter.jetty :as jetty]
    [compojure.core :refer [POST defroutes]]
    [voice-fn.pipeline :refer [process-frame]]))

(defn twilio-webhook-handler [pipeline req]
  ;; Handle incoming Twilio webhook
  (let [audio-data (-> req :body :audio)]
    (a/put! (:pipeline/main-ch @pipeline)
            (frames/audio-input-frame audio-data))))

(defroutes telephony-routes
  (POST "/twilio/webhook" [] #(twilio-webhook-handler pipeline %)))

(defmethod process-frame :transport/telephony
  [processor-type pipeline config frame]
  (case (:frame/type frame)
    :system/start
    (let [port (get-in config [:processor/config :port] 8081)
          server (jetty/run-jetty telephony-routes {:port port :join? false})]
      (swap! pipeline assoc-in [:transport/telephony :server] server))

    :system/stop
    (when-let [server (get-in @pipeline [:transport/telephony :server])]
      (.stop server))))
#+end_src

Would you like me to elaborate on any part of this design?

I would like to change to use reitit instead of compojure

Here's how we can restructure the WebSocket transport using reitit:

#+begin_src clojure
;; src/voice_fn/transport/websocket.clj
(ns voice-fn.transport.websocket
  (:require
   [org.httpkit.server :as http]
   [reitit.ring :as ring]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.swagger :as swagger]
   [reitit.swagger-ui :as swagger-ui]
   [muuntaja.core :as m]
   [voice-fn.pipeline :refer [process-frame]]
   [clojure.core.async :as a]))

(defn websocket-handler [pipeline]
  (fn [req]
    (http/with-channel req channel
      (let [client-id (random-uuid)]
        ;; Store WS connection in pipeline state
        (swap! pipeline assoc-in [:transport/websocket :connections client-id] channel)

        ;; Handle incoming messages
        (http/on-receive channel
          (fn [data]
            (a/put! (:pipeline/main-ch @pipeline)
                    (frames/audio-input-frame data))))

        ;; Handle client disconnect
        (http/on-close channel
          (fn [status]
            (swap! pipeline update-in [:transport/websocket :connections] dissoc client-id)))))))

(defn app-routes [pipeline]
  ["/api"
   ["/swagger.json"
    {:get {:no-doc true
           :handler (swagger/create-swagger-handler)}}]

   ["/ws"
    {:get {:handler (websocket-handler pipeline)
           :summary "WebSocket endpoint for audio streaming"}}]])

(def app-middleware
  [muuntaja/format-middleware])

(defn create-app [pipeline]
  (ring/ring-handler
   (ring/router
    [(app-routes pipeline)]
    {:data {:muuntaja m/instance
            :middleware app-middleware}})
   (ring/routes
    (swagger-ui/create-swagger-ui-handler
     {:path "/api/swagger"})
    (ring/create-default-handler))))

(defmethod process-frame :transport/websocket
  [processor-type pipeline config frame]
  (case (:frame/type frame)
    :system/start
    (let [port (get-in config [:processor/config :port] 8080)
          app (create-app pipeline)
          server (http/run-server app {:port port})]
      (swap! pipeline assoc-in [:transport/websocket :server] server))

    :system/stop
    (when-let [server (get-in @pipeline [:transport/websocket :server])]
      (server)) ; Stop the server

    :audio/output
    (doseq [[_ conn] (get-in @pipeline [:transport/websocket :connections])]
      (http/send! conn (:data frame)))))

;; Usage in pipeline config remains the same:
(def pipeline
  {:pipeline/config {...}
   :pipeline/processors
   [{:processor/type :transport/websocket
     :processor/accepted-frames #{:system/start :system/stop :audio/output}
     :processor/generates-frames #{:audio/raw-input}
     :processor/config {:port 8080}}
    ...]})
#+end_src

Key improvements with reitit:
1. Better route organization with data-driven routing
2. Built-in support for OpenAPI/Swagger documentation
3. Better middleware handling
4. More extensible routing system

Would you like me to:
1. Add more endpoints (like health checks)?
2. Show how to add authentication middleware?
3. Add proper error handling?
4. Show how to structure the telephony routes in the same system?

*** This is fine but don't you agree that this adds too much to the model? I think it might be better to just create the websocket connection outside the pipeline config and just give that as configuration to the transport input

Absolutely! You make a great point. This would better follow the principle of separation of concerns and keep the pipeline focused on processing frames rather than managing servers. Here's the refined approach:

#+begin_src clojure

;; src/voice_fn/server.clj
(ns voice-fn.server
  (:require
   [org.httpkit.server :as http]
   [reitit.ring :as ring]
   [voice-fn.pipeline :as pipeline]))

(defn websocket-handler [pipeline-atom]
  (fn [req]
    (http/with-channel req channel
      (let [client-id (random-uuid)]
        (swap! pipeline-atom assoc-in [:transport/websocket :connections client-id] channel)
        (http/on-receive channel
          (fn [data]
            (pipeline/send-frame! pipeline-atom (frames/audio-input-frame data))))
        (http/on-close channel
          (fn [status]
            (swap! pipeline-atom update-in [:transport/websocket :connections] dissoc client-id)))))))

;; Server setup happens outside pipeline
(defn start-server! [pipeline-atom port]
  (let [app (ring/ring-handler
             (ring/router
              [["/ws" {:get {:handler (websocket-handler pipeline-atom)}}]]))
        server (http/run-server app {:port port})]
    server))

;; The pipeline processor becomes much simpler
(defmethod pipeline/process-frame :transport/websocket
  [_ pipeline _ frame]
  (case (:frame/type frame)
    :audio/output
    (doseq [[_ conn] (get-in @pipeline [:transport/websocket :connections])]
      (http/send! conn (:data frame)))
    nil))

;; Usage:
(comment
  ;; Start server separately
  (def server (start-server! pipeline-atom 8080))

  ;; Pipeline config focuses only on processing
  (def pipeline-config
   {:pipeline/processors
    [{:processor/type :transport/websocket
      :processor/accepted-frames #{:audio/output}
      :processor/generates-frames #{:audio/raw-input}}
     ...]}))
#+end_src

This is much cleaner because:
1. Server lifecycle is managed separately from the pipeline
2. Pipeline only handles frame processing
3. Clearer separation between web concerns and pipeline concerns
4. More flexible - can have multiple transport inputs feeding the same pipeline

What do you think about this approach?

*** Here's an interesting idea from the pipecat project. They have a constant websocket transport that receives a websocket connection as config for the transport and a serializer/deserializer. This is interesting because the websocket transport works the same for different providers, it is just that serializing and deserializing frames for the pipeline will be different.

That's an excellent pattern! It separates the transport mechanism from the protocol-specific details. Let's implement this in Clojure:

#+begin_src clojure

;; src/voice_fn/transport/websocket/protocols.clj
(ns voice-fn.transport.websocket.protocols)

(defprotocol FrameSerializer
  (serialize-frame [this frame])
  (deserialize-frame [this raw-data]))

;; src/voice_fn/transport/websocket/serializers.clj
(ns voice-fn.transport.websocket.serializers
  (:require [voice-fn.transport.websocket.protocols :as p]
            [voice-fn.frames :as frames]))

;; Example Twilio serializer
(defrecord TwilioSerializer [stream-sid]
  p/FrameSerializer
  (serialize-frame [_ frame]
    ;; Convert pipeline frame to Twilio-specific format
    (case (:frame/type frame)
      :audio/output {:event "media"
                     :streamSid stream-sid
                     :media {:payload (:data frame)}}
      ;; ... other frame types
      ))

  (deserialize-frame [_ raw-data]
    ;; Convert Twilio message to pipeline frame
    (let [data (json/parse-string raw-data true)]
      (case (:event data)
        "media" (frames/audio-input-frame (:payload (:media data)))
        ;; ... other event types
        ))))

;; src/voice_fn/transport/websocket/transport.clj
(ns voice-fn.transport.websocket.transport
  (:require [voice-fn.pipeline :as pipeline]
            [clojure.core.async :as a]))

(defmethod pipeline/process-frame :transport/websocket
  [_ pipeline {:keys [websocket serializer]} frame]
  (case (:frame/type frame)
    :audio/output
    (let [serialized (p/serialize-frame serializer frame)]
      (http/send! websocket serialized))

    :audio/raw-input
    (when-let [data (:data frame)]
      (let [deserialized (p/deserialize-frame serializer data)]
        (a/put! (:pipeline/main-ch @pipeline) deserialized)))

    nil))

;; Usage example:
(comment
  ;; In your server setup
  (defn websocket-handler [pipeline-atom]
    (fn [req]
      (http/with-channel req channel
        (let [stream-sid (get-in req [:params :stream-sid])
              serializer (->TwilioSerializer stream-sid)]
          ;; Add websocket connection and its serializer to pipeline config
          (swap! pipeline-atom update :pipeline/processors conj
                 {:processor/type :transport/websocket
                  :processor/config {:websocket channel
                                   :serializer serializer}
                  :processor/accepted-frames #{:audio/output}
                  :processor/generates-frames #{:audio/raw-input}})))))

  ;; Different serializer for different provider
  (defrecord WebRTCSerializer []
    p/FrameSerializer
    (serialize-frame [_ frame]
      ;; WebRTC-specific serialization
      )
    (deserialize-frame [_ raw-data]
      ;; WebRTC-specific deserialization
      )))

#+end_src

Key benefits of this approach:
1. Transport logic is protocol-agnostic
2. Easy to add new providers by implementing new serializers
3. Clean separation between transport mechanism and protocol details
4. Testable - can test serializers independently
5. Flexible - can swap serializers at runtime

Would you like me to:
1. Add more example serializers?
2. Show how to handle different types of frames?
3. Add error handling for malformed messages?

***

```
