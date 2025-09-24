package il.co.dotcore.julian.fpinscala2023.ch07

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.time.Duration
import java.util.concurrent.TimeoutException

class ParSuite extends munit.FunSuite {
  import fpinscala.parallelism.Par.*

  // ex03
  test("map2") {
    import Map2.*
    // import Map2.AnswersKey.*

    assert(
      unit(10)
        .map2(unit(20))(_ + _)
        .run(Executors.newFixedThreadPool(3))
        .get(2, TimeUnit.SECONDS) == 30
    )

    assert(
      lazyUnit {
        Thread.sleep(Duration.ofMillis(300))
        10
      }
        .map2(lazyUnit {
          Thread.sleep(Duration.ofMillis(200))
          20
        })(_ + _)
        .run(Executors.newFixedThreadPool(3))
        .get(1, TimeUnit.SECONDS) == 30
    )

    assert({
      // confirm failure by changing to RuntimeException
      intercept[TimeoutException](
        lazyUnit {
          Thread.sleep(Duration.ofMillis(500))
          10
        }
          .map2(lazyUnit {
            Thread.sleep(Duration.ofMillis(700))
            20
          })(_ + _)
          .run(Executors.newFixedThreadPool(1))
          .get(1000, TimeUnit.MILLISECONDS)
      ).getMessage == null
    })

    assert({
      // confirm failure by changing to RuntimeException
      intercept[TimeoutException](
        lazyUnit {
          Thread.sleep(Duration.ofMillis(700))
          10
        }
          .map2(lazyUnit {
            Thread.sleep(Duration.ofMillis(500))
            20
          })(_ + _)
          .run(Executors.newFixedThreadPool(1))
          .get(1000, TimeUnit.MILLISECONDS)
      ).getMessage == null
    })
  }

  // ex04
  test("asyncF") {
    assert(
      asyncF((n: Double) => math.sqrt(n))(9879)
        .run(Executors.newFixedThreadPool(2))
        .get == 99.39315871829409
    )
  }

  // ex05
  test("sequence") {
    assert(
      sequence(List(unit(1), unit(2), unit(3)))
        .run(Executors.newFixedThreadPool(3))
        .get == List(1, 2, 3)
    )
  }

  // ex06
  test("filter") {
    assert(
      parFilter(List.range(1, 20))(_ % 2 == 0)
        .run(Executors.newFixedThreadPool(2))
        .get == List(2, 4, 6, 8, 10, 12, 14, 16, 18)
    )
  }
}
