use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "package_versions")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i64,
    pub package_id: i64,
    pub version: String,
    pub checksum_sha256: String,
    pub blob_key: String,
    pub size_bytes: i64,
    pub yanked: i32,
    pub sema_version_req: Option<String>,
    pub tarball_url: Option<String>,
    pub published_at: String,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::package::Entity",
        from = "Column::PackageId",
        to = "super::package::Column::Id"
    )]
    Package,
    #[sea_orm(has_many = "super::dependency::Entity")]
    Dependencies,
}

impl Related<super::package::Entity> for Entity {
    fn to() -> RelationDef { Relation::Package.def() }
}
impl Related<super::dependency::Entity> for Entity {
    fn to() -> RelationDef { Relation::Dependencies.def() }
}

impl ActiveModelBehavior for ActiveModel {}
