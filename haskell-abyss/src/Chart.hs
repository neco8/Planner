module Chart (getChart) where
import           ActionPriorityMatrix                      (ActionPriorityMatrix,
                                                            Effort, Impact,
                                                            effort, impact,
                                                            runEffort,
                                                            runImpact)
import           Data.Vector                               (Vector, toList)
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy             (Default (def), navy,
                                                            opaque, plot,
                                                            points, setColors,
                                                            to, (^.))

values :: Monad m => m (ActionPriorityMatrix qwa) -> m (Float, Float)
values apms = do
  apm <- apms
  let i = apm ^. impact . to runImpact
      e = apm ^. effort . to runEffort
  pure (e, i)

getChart :: FilePath -> [ActionPriorityMatrix qwa] -> IO ()
getChart path d = toFile def path $ do
  setColors [ opaque navy ]
  plot $ points "Action Priority Matrix" $ values d
