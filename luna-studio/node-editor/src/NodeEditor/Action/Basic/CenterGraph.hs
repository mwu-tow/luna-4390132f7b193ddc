module NodeEditor.Action.Basic.CenterGraph where

import           Common.Prelude                             hiding (span)
import           Data.Matrix                                (multStd2)
import           LunaStudio.Data.CameraTransformation       (lastInverse, logicalToScreen, screenToLogical)
import           LunaStudio.Data.Position                   (minimumRectangle, vector, x, y)
import           LunaStudio.Data.Size                       (Size (Size))
import           LunaStudio.Data.Vector2                    (Vector2 (Vector2), scalarProduct)
import           NodeEditor.Action.Basic.ModifyCamera       (resetCamera)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, modifyNodeEditor)
import           NodeEditor.Action.State.Scene              (getScreenSize)
import           NodeEditor.Data.Matrix                     (invertedScaleMatrix, invertedTranslationMatrix, scaleMatrix, translationMatrix)
import           NodeEditor.React.Model.Node.ExpressionNode (position)
import           NodeEditor.React.Model.NodeEditor          (screenTransform)
import           NodeEditor.State.Global                    (State)


padding :: Vector2 Double
padding = Vector2 80 80

centerGraph :: Command State ()
centerGraph = do
    nodes <- getExpressionNodes
    case minimumRectangle $ map (view position) nodes of
        Just (leftTop, rightBottom) -> withJustM getScreenSize $ \screenSize -> do
            let span         = Size (rightBottom ^. vector - leftTop ^. vector + scalarProduct padding 2)
                shift        = scalarProduct (leftTop ^. vector - rightBottom ^. vector) 0.5 - leftTop ^. vector
                factor       = min 1 $ min (screenSize ^. x / span ^. x) (screenSize ^. y / span ^. y)
            modifyNodeEditor $ do
                screenTransform . logicalToScreen .= multStd2 (translationMatrix shift) (scaleMatrix factor)
                screenTransform . screenToLogical .= multStd2 (invertedScaleMatrix factor) (invertedTranslationMatrix shift)
                screenTransform . lastInverse     .= 2
        Nothing -> resetCamera
