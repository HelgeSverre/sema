pub mod api;
pub mod auth;
pub mod blob;
pub mod config;
pub mod db;
pub mod github;
pub mod web;

use axum::{
    routing::{delete, get, post, put},
    Router,
};
use std::sync::Arc;
use tower_http::services::ServeDir;

pub struct AppState {
    pub db: db::Db,
    pub config: config::Config,
}

pub fn build_router(state: Arc<AppState>) -> Router {
    Router::new()
        .route("/healthz", get(|| async { "ok" }))
        // Web pages
        .route("/", get(web::index))
        .route("/search", get(web::search))
        .route("/packages/{name}", get(web::package_detail))
        .route("/login", get(web::login))
        .route("/account", get(web::account))
        // GitHub OAuth
        .route("/auth/github", get(github::start))
        .route("/auth/github/callback", get(github::callback))
        // Static files
        .nest_service("/static", ServeDir::new("static"))
        // Auth API
        .route("/api/v1/auth/register", post(api::auth::register))
        .route("/api/v1/auth/login", post(api::auth::login))
        .route("/api/v1/auth/logout", post(api::auth::logout))
        // Tokens API
        .route(
            "/api/v1/tokens",
            post(api::tokens::create).get(api::tokens::list),
        )
        .route("/api/v1/tokens/{id}", delete(api::tokens::revoke))
        // Packages API
        .route("/api/v1/packages/{name}", get(api::packages::get_package))
        .route(
            "/api/v1/packages/{name}/{version}",
            put(api::packages::publish),
        )
        .route(
            "/api/v1/packages/{name}/{version}/download",
            get(api::packages::download),
        )
        .route(
            "/api/v1/packages/{name}/{version}/yank",
            post(api::packages::yank),
        )
        .route(
            "/api/v1/packages/{name}/owners",
            get(api::packages::list_owners)
                .put(api::packages::add_owner)
                .delete(api::packages::remove_owner),
        )
        .route("/api/v1/search", get(api::packages::search))
        .with_state(state)
}
