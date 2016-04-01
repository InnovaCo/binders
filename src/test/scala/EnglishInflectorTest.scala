/*
 * Copyright 2016 Innova Co s.a r.l
 *
 * Modified by Innova Co s.a r.l from original sources
 * located at: https://github.com/atteo/evo-inflector
 *
 * Copyright 2011 Atteo.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

import eu.inn.binders.naming.English
import org.scalatest.{FlatSpec, Matchers}

class EnglishInflectorTest extends FlatSpec with Matchers {

  final private val inflector: English = new English
  val exampleWords = Map(
    ("alga", "algae"),
    ("nova", "novas"),
    ("dogma", "dogmas"),
    ("Woman", "Women"),
    ("church", "churches"),
    ("quick_chateau", "quick_chateaus"),
    ("codex", "codices"),
    ("index", "indexes"),
    ("NightWolf", "NightWolves"),
    ("Milieu", "Milieus"),
    ("basis", "bases"),
    ("iris", "irises"),
    ("phalanx", "phalanxes"),
    ("tempo", "tempos"),
    ("foot", "feet"),
    ("series", "series"),
    ("WorldAtlas", "WorldAtlases"),
    ("wish", "wishes"),
    ("Bacterium", "Bacteria"),
    ("medium", "mediums"),
    ("Genus", "Genera"),
    ("stimulus", "stimuli"),
    ("opus", "opuses"),
    ("status", "statuses"),
    ("Box", "Boxes"),
    ("ferry", "ferries"),
    ("protozoon", "protozoa"),
    ("cherub", "cherubs"),
    ("human", "humans"),
    ("sugar", "sugar"),
    ("virus", "viruses"),
    ("gastrostomy", "gastrostomies"),
    ("baculum", "bacula"),
    ("pancreas", "pancreases"),
    ("todo", "todos"),
    ("status", "statuses")
  )

  "EnglishInflector " should " pluralize exampleWordList" in {
    exampleWords.foreach { kv ⇒
      kv._2 shouldBe inflector.getPlural(kv._1)
    }
  }

  "EnglishInflector " should " pluralize withCount" in {
    "cat" shouldBe inflector.getPlural("cat", 1)
    "cats" shouldBe inflector.getPlural("cat", 2)
    "demoness" shouldBe inflector.getPlural("demoness", 1)
    "demonesses" shouldBe inflector.getPlural("demoness", 2)
  }

  "EnglishInflector staticMethods " should " pluralize" in {
    "sulfimides" shouldBe English.plural("sulfimide")
    "semifluids" shouldBe English.plural("semifluid", 2)
  }

  "EnglishInflector " should " singularize exampleWordList" in {
    exampleWords.foreach { kv ⇒
      kv._1 shouldBe inflector.getSingular(kv._2)
    }
  }

  "EnglishInflector staticMethods " should " singularize" in {
    "sulfimide" shouldBe English.singular("sulfimides")
    "semifluid" shouldBe English.singular("semifluids")
  }
}