(defproject io.simplect/compose			"0.6"
  :description					"Clojure library to aid composing functions"
  :license	{:name				"Eclipse Public License"
                 :url				"http://www.eclipse.org/legal/epl-v10.html"
                 :contact			"Klaus Harbo"
                 :email				"klaus@harbo.net -and- kh@harbo-enterprises.com"}
  :jar-name					"io.simplect.compose-%s.jar"
  :target-path					"target/%s"
  :repositories					[["he-pub"  {:url "s3p://clojure.harbo-enterprises.com/public/releases/", :creds :gpg}]]
  :profiles					{:uberjar {:aot :all}}
  :plugins					[[lein-tools-deps "0.4.1"]]
  :middleware					[lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config			{:config-files [:install :user :project]}
  )

