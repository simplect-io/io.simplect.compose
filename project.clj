(defproject io.simplect/functional		"0.5"
  :description					"Library to aid functional programming"
  :license	{:name				"Copyright (c) 2017-2019 Klaus Harbo. All rights reserved."
                 :contact			"Klaus Harbo"
                 :email				"klaus@harbo.net"}
  :jar-name					"io.simplect.functional-%s.jar"
  :target-path					"target/%s"
  :repositories					[["he-pub"  {:url "s3p://clojure.harbo-enterprises.com/public/releases/", :creds :gpg}]
                                                 ["he-priv" {:url "s3p://clojure.harbo-enterprises.com/releases/", :creds :gpg}]]
  :profiles					{:uberjar {:aot :all}}
  :plugins					[[lein-tools-deps "0.4.1"]]
  :middleware					[lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config			{:config-files [:install :user :project]}
  )

