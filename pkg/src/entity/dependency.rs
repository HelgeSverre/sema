use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "dependencies")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i64,
    pub version_id: i64,
    pub dependency_name: String,
    pub version_req: String,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::package_version::Entity",
        from = "Column::VersionId",
        to = "super::package_version::Column::Id"
    )]
    Version,
}

impl Related<super::package_version::Entity> for Entity {
    fn to() -> RelationDef { Relation::Version.def() }
}

impl ActiveModelBehavior for ActiveModel {}
