naver-translate
===============

Haskell interface to [Naver Translate][1].  Note that Naver Translate doesn't
provide any official API.

    > :set -XOverloadedStrings
    > import Language.Translate.Naver (translate)
    > import Data.LanguageCodes (ISO639_1(JA, KO))
    > import Data.Text (unpack)
    > result <- translate KO JA "안녕하세요."
    > putStrLn (unpack result)
    こんにちは

Written by [Hong Minhee][2] and distributed under [GPLv3][3] or higher.
See also LICENSE file.

[1]: http://translate.naver.com/
[2]: http://hongminhee.org/
[3]: http://www.gnu.org/licenses/gpl-3.0.html
