 #| BULK library
    Copyright (C) 2013--2018 Pierre Thierry <pierre@nothos.net>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. |#

(uiop:define-package :bulk/vocabularies/hash
  (:use :cl :bulk/write :bulk/reference :bulk/words :flexi-streams)
  (:import-from :ironclad))

(in-package :bulk/vocabularies/hash)

(defun main (&optional (path "hash.bulk"))
  (let* ((content
		  (with-output-to-sequence (out)
			(write-bulk out "hash")
			(write-bulk out "The forms in this vocabulary can be used to represent hashes along with the hashing algorithm instead of using an unmarked byte sequence. When an algorithm has other inputs than the message, they can be provided after the hash itself as a property list.

When an algorithm can produce hashes in different sizes and the size used is a number of bits divisible by 8, the size property should be omitted from the property list and inferred by the processing application from the size of the BULK expression (e.g. `( sha3 # 24 {hash} )` is a 196-bits SHA3 hash).

As a rule, each of these forms can contain `nil` as a first expression to denote not a hash but a choice of configuration in some application context. For example, `( uuid nil prepend {ns} )` could mean that subsequent v3 and v5 UUIDs will be produced with {ns} as UUID namespace.")
			(write-bulk out (list (ref #x20 #xA) :nil "size"))
			(write-bulk out (list (ref #x20 #xA) :nil "prepend"))
			(write-bulk out (list (ref #x20 #xA) :nil "append"))
			(write-bulk out (list (ref #x20 #xA) :nil "key"))
			(write-bulk out (list (ref #x20 #xA) :nil "rounds"))

			(write-bulk out (list (ref #x20 #xA) :nil "bsd" "( bsd Word16 )"))
			(write-bulk out (list (ref #x20 #xA) :nil "sysv" "( sysv Word16 )"))
			(write-bulk out (list (ref #x20 #xA) :nil "crc" "( crc Bytes )"))
			(write-bulk out (list (ref #x20 #xA) :nil "crc32" "( crc32 Bytes )" (list (ref #x20 #x11) (list (ref #x28 7) (list (ref #x20 #x12) 0) (ref #x28 3) #x04C11DB7))))
			(write-bulk out (list (ref #x20 #xA) :nil "crc64" "( crc64 Bytes )" (list (ref #x20 #x11) (list (ref #x28 7) (list (ref #x20 #x12) 0) (ref #x28 3) (arbitrary-bytes (cons 7 (word->bytes #x1B :length 8)))))))
			(write-bulk out (list (ref #x20 #xA) :nil "fletcher" "( fletcher Word {config} )"))
			(write-bulk out (list (ref #x20 #xA) :nil "adler32" "( adler32 Word32 )" (list (ref #x20 #x11) (list (ref #x28 #xA) (list (ref #x20 #x12) 0) (ref #x28 3) 65521))))
			(write-bulk out (list (ref #x20 #xA) :nil "pjwhash" "( pjw Word )"))
			(write-bulk out (list (ref #x20 #xA) :nil "elfhash" "( fnv Word )" ))

			(write-bulk out (list (ref #x20 #xA) :nil "murmur1" "( murmur1 Word )"))
			(write-bulk out (list (ref #x20 #xA) :nil "murmur2" "( murmur2 Word )"))
			(write-bulk out (list (ref #x20 #xA) :nil "murmur2a" "( murmur2a Word )"))
			(write-bulk out (list (ref #x20 #xA) :nil "murmur64a" "( murmur64a Word )"))
			(write-bulk out (list (ref #x20 #xA) :nil "murmur64b" "( murmur64b Word )"))
			(write-bulk out (list (ref #x20 #xA) :nil "murmur3" "( murmur3 Word )"))

			(write-bulk out (list (ref #x20 #xA) :nil "umac" "( umac Word {config} )"))
			(write-bulk out (list (ref #x20 #xA) :nil "vmac" "( vmac Word {config} )"))

			(write-bulk out (list (ref #x20 #xA) :nil "uuid" "( uuid Word128 {config} )"))
			(write-bulk out (list (ref #x20 #xA) :nil "md2" "( md2 Word128 )"))
			(write-bulk out (list (ref #x20 #xA) :nil "md4" "( md4 Word128 )"))
			(write-bulk out (list (ref #x20 #xA) :nil "md5" "( md5 Word128 )"))
			(write-bulk out (list (ref #x20 #xA) :nil "md6" "( md6 Bytes {config} )"))
			(write-bulk out (list (ref #x20 #xA) :nil "ripemd" "( ripemd Bytes )"))
			(write-bulk out (list (ref #x20 #xA) :nil "haval" "( haval Bytes )"))
			(write-bulk out (list (ref #x20 #xA) :nil "gost" "( gost Array )"))
			(write-bulk out (list (ref #x20 #xA) :nil "sha1" "( sha1 Array )"))
			(write-bulk out (list (ref #x20 #xA) :nil "sha2" "( sha2 Array )"))
			(write-bulk out (list (ref #x20 #xA) :nil "sha3" "( sha3 Array )"))
			(write-bulk out (list (ref #x20 #xA) :nil "shake128" "( sha3 Bytes )"))
			(write-bulk out (list (ref #x20 #xA) :nil "shake256" "( sha3 Bytes )"))
			(write-bulk out (list (ref #x20 #xA) :nil "tiger" "( tiger Bytes )"))
			(write-bulk out (list (ref #x20 #xA) :nil "tiger2" "( tiger2 Bytes )"))
			(write-bulk out (list (ref #x20 #xA) :nil "whirlpool" "( whirlpool Array )"))
			(write-bulk out (list (ref #x20 #xA) :nil "blake" "( blake Array )"))
			(write-bulk out (list (ref #x20 #xA) :nil "blake2" "( blake2 Bytes )"))))
		 (hash (word* (ironclad:digest-sequence (ironclad:make-digest :shake128 :output-length 8) content))))
	(with-open-file (out path :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
	  (write-bulk out (list (ref #x20 #x0) 1 0))
	  (write-bulk out (list (ref #x20 #xC) #x28 (list (ref #x28 #x22) hash) (arbitrary-bytes content))))))
