package eu.inn.binders

package object dynamic {
    implicit class DynamicReader(val dynamic: DynamicObject) {
      def fromDynamic[O]: O = macro DynamiObjectMacro.fromDynamic[O]
    }

    implicit class DynamicGenerator[O](val obj: O) {
      def toDynamic: DynamicObject = macro DynamiObjectMacro.toDynamic[O]
    }
}
