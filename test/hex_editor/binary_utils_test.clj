(ns hex-editor.binary-utils-test
  (:require [clojure.test :refer :all])
  (:require [hex-editor.binary-utils :refer [apply-conversions type-conversions]]))


(deftest apply-conversions-test
  (is (= 65 ((:int type-conversions) (byte 65))))
  (is (= 65 ((:uint type-conversions) (byte 65))))
  (is (= 65 ((:uint8 type-conversions) (byte 65))))
  (is (= "A" ((:char type-conversions) (byte 65))))
  (is (= "A" ((:ascii type-conversions) (byte-array [(byte 65)]))))
  (is (= "A" ((:utf-8 type-conversions) (byte-array [(byte 65)]))))
  )
