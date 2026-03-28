use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "packages")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i64,
    #[sea_orm(unique)]
    pub name: String,
    pub description: String,
    pub repository_url: Option<String>,
    pub source: String,
    pub github_repo: Option<String>,
    pub webhook_secret: Option<String>,
    pub created_at: String,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::package_version::Entity")]
    Versions,
    #[sea_orm(has_many = "super::owner::Entity")]
    Owners,
    #[sea_orm(has_many = "super::github_sync_log::Entity")]
    SyncLogs,
}

impl Related<super::package_version::Entity> for Entity {
    fn to() -> RelationDef { Relation::Versions.def() }
}
impl Related<super::owner::Entity> for Entity {
    fn to() -> RelationDef { Relation::Owners.def() }
}
impl Related<super::github_sync_log::Entity> for Entity {
    fn to() -> RelationDef { Relation::SyncLogs.def() }
}

impl ActiveModelBehavior for ActiveModel {}
