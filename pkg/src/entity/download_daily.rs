use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "download_daily")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub package_name: String,
    #[sea_orm(primary_key, auto_increment = false)]
    pub version: String,
    #[sea_orm(primary_key, auto_increment = false)]
    pub download_date: String,
    pub count: i32,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
