module Chart (getChart) where
import           ActionPriorityMatrix                      (ActionPriorityMatrix,
                                                            Effort, Impact,
                                                            effort, impact,
                                                            runEffort,
                                                            runImpact)
import           Data.Vector                               (Vector, toList)
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy             (Default (def),
                                                            axis_grid,
                                                            axis_labels,
                                                            axis_ticks,
                                                            axis_viewport,
                                                            laxis_override,
                                                            layout_x_axis,
                                                            layout_y_axis, navy,
                                                            opaque, plot,
                                                            plot_points_title,
                                                            points, setColors,
                                                            to, vmap, (.=),
                                                            (.~), (^.))

values :: Monad m => m (ActionPriorityMatrix qwa) -> m (Float, Float)
values apms = do
  apm <- apms
  let i = apm ^. impact . to runImpact
      e = apm ^. effort . to runEffort
  pure (e, i)

getChart :: FilePath -> [ActionPriorityMatrix qwa] -> IO ()
getChart path d = toFile def path $ do
  setColors [ opaque navy ]
  layout_x_axis . laxis_override .=
    ((axis_viewport .~ vmap (0, 10)) .
      (axis_grid .~ [0..10]) .
      (axis_ticks .~ ((\n -> (n, if n `elem` bigLabels then 4 else 2)) <$> [0, 0.2..10])) .
      (axis_labels .~ [(\n -> (n, show n)) <$> [0, 2..10]])
    )
  layout_y_axis . laxis_override .=
    ((axis_viewport .~ vmap (0, 10)) .
      (axis_grid .~ [0..10]) .
      (axis_ticks .~ ((\n -> (n, if n `elem` bigLabels then 4 else 2)) <$> [0, 0.2..10]))) .
      (axis_labels .~ [(\n -> (n, show n)) <$> [0, 2..10]])
  plot $ points "Action Priority Matrix" $ values d
  where
    bigLabels = [0.0, 2.0..10.0]
