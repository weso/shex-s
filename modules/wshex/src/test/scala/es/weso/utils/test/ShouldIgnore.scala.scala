package es.weso.utils.test

sealed abstract class ShouldIgnoreOption
case object IgnoreTest extends ShouldIgnoreOption
case object DontIgnore extends ShouldIgnoreOption



