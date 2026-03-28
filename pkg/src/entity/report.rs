use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "reports")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i64,
    pub reporter_id: i64,
    pub target_type: String,
    pub target_name: String,
    pub report_type: String,
    pub reason: String,
    pub status: String,
    pub resolved_by: Option<i64>,
    pub resolved_at: Option<String>,
    pub created_at: String,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::user::Entity",
        from = "Column::ReporterId",
        to = "super::user::Column::Id"
    )]
    Reporter,
}

impl Related<super::user::Entity> for Entity {
    fn to() -> RelationDef { Relation::Reporter.def() }
}

impl ActiveModelBehavior for ActiveModel {}
