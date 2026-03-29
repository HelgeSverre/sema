use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel)]
#[sea_orm(table_name = "users")]
pub struct Model {
    #[sea_orm(primary_key)]
    pub id: i64,
    #[sea_orm(unique)]
    pub username: String,
    #[sea_orm(unique)]
    pub email: String,
    pub password_hash: Option<String>,
    #[sea_orm(unique)]
    pub github_id: Option<i64>,
    pub homepage: Option<String>,
    pub is_admin: i32,
    pub banned_at: Option<String>,
    pub created_at: String,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(has_many = "super::session::Entity")]
    Sessions,
    #[sea_orm(has_many = "super::api_token::Entity")]
    ApiTokens,
    #[sea_orm(has_many = "super::owner::Entity")]
    Owners,
    #[sea_orm(has_many = "super::oauth_connection::Entity")]
    OauthConnections,
    #[sea_orm(has_many = "super::report::Entity")]
    Reports,
}

impl Related<super::session::Entity> for Entity {
    fn to() -> RelationDef { Relation::Sessions.def() }
}
impl Related<super::api_token::Entity> for Entity {
    fn to() -> RelationDef { Relation::ApiTokens.def() }
}
impl Related<super::owner::Entity> for Entity {
    fn to() -> RelationDef { Relation::Owners.def() }
}
impl Related<super::oauth_connection::Entity> for Entity {
    fn to() -> RelationDef { Relation::OauthConnections.def() }
}
impl Related<super::report::Entity> for Entity {
    fn to() -> RelationDef { Relation::Reports.def() }
}

impl ActiveModelBehavior for ActiveModel {}
