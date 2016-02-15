namespace HomeDepot

module Model =

    type Attributes = Map<string,string>

    type Product

    type Observation

    type Relevance = float
    type Example = Relevance * Observation

    type Predictor = Observation -> Relevance

    type Learner = Example [] -> Predictor

    val trainset : Example []
    val testset : Observation []
    val attributes: Map<string,Set<string>>

    val rmse : (Relevance * Relevance) seq -> float

    type Quality
    val evaluate : int -> Learner -> Quality

    val createSubmission : Learner -> unit
