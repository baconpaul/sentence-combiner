# combiner

A clojure library which takes sentences where the relationships are given and
generates a set of potential answers, some correct some incorrect, which students
may give. It also realizes which are good and bad so student feedback can be generated.

It's also woefully incomplete, and really only does a few things so we can discuss it.

## Usage

* Install clojure and leiningen. This is most easily done with "brew install leiningen" on macos.
* In the main directory of the project (the one containing project.clj) type "lein test". This should
show tests passing.
* Then if they pass do "lein run"
* Then in your favorite clojure repl environment play around with core.clj and the libraries

## License

Copyright © 2017 Paul Walker

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
