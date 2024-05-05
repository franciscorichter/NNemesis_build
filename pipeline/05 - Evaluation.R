load("~/Library/CloudStorage/Dropbox/github/other_emphasis_versions/emphasisLD/data/FamilyBirdTrees.Rdata")
tree = FamilyBirdTrees$Parulidae$tree

p_dropout = 0


#DDD_est(tree = tree, cnn_ltt = neural_net$model())



DDD_est(tree = tree, cnn_ltt = cnn_ltt)




library(NNemesis)

tree = FamilyMammalTrees$Ziphiidae$tree
p_dropout = 0

DDD_est(tree = tree,cnn_ltt = neural_net$model)
